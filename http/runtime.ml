let src = Logs.Src.create "runtime"

module Log = (val Logs.src_log src : Logs.LOG)

module type S = sig
  type t

  val next_read_operation : t -> [ `Read | `Yield | `Close ]
  val read : t -> Bigstringaf.t -> off:int -> len:int -> int
  val read_eof : t -> Bigstringaf.t -> off:int -> len:int -> int
  val yield_reader : t -> (unit -> unit) -> unit

  val next_write_operation :
    t -> [ `Write of Bigstringaf.t Faraday.iovec list | `Close of int | `Yield ]

  val report_write_result : t -> [ `Ok of int | `Closed ] -> unit
  val yield_writer : t -> (unit -> unit) -> unit
  val report_exn : t -> exn -> unit
end

module Buffer : sig
  type t

  val create : int -> t
  val get : t -> f:(Bigstringaf.t -> off:int -> len:int -> int) -> int
  val put : t -> f:(Bigstringaf.t -> off:int -> len:int -> int) -> int
end = struct
  type t = { mutable buffer: Bigstringaf.t; mutable off: int; mutable len: int }

  let create size =
    let buffer = Bigstringaf.create size in
    { buffer; off= 0; len= 0 }

  let compress t =
    if t.len = 0 then begin
      t.off <- 0;
      t.len <- 0
    end
    else if t.off > 0 then begin
      Bigstringaf.blit t.buffer ~src_off:t.off t.buffer ~dst_off:0 ~len:t.len;
      t.off <- 0
    end

  let get t ~f =
    let n = f t.buffer ~off:t.off ~len:t.len in
    t.off <- t.off + n;
    t.len <- t.len - n;
    if t.len = 0 then t.off <- 0;
    n

  let put t ~f =
    compress t;
    let off = t.off + t.len in
    let buf = t.buffer in
    if Bigstringaf.length buf = t.len then begin
      t.buffer <- Bigstringaf.create (2 * Bigstringaf.length buf);
      Bigstringaf.blit buf ~src_off:t.off t.buffer ~dst_off:0 ~len:t.len
    end;
    let n = f t.buffer ~off ~len:(Bigstringaf.length t.buffer - off) in
    t.len <- t.len + n;
    n
end

exception Flow of string

let rec terminate orphans =
  match Miou.care orphans with
  | None -> Miou.yield ()
  | Some None -> Miou.yield (); terminate orphans
  | Some (Some prm) -> Miou.await_exn prm; terminate orphans

type _ Effect.t += Spawn : (unit -> unit) -> unit Effect.t

let flat_tasks fn =
  let orphans = Miou.orphans () in
  let open Effect.Deep in
  let retc = Fun.id
  and exnc = raise
  and effc : type c. c Effect.t -> ((c, 'a) continuation -> 'a) option =
    function
    | Spawn fn ->
        let _ = Miou.call_cc ~orphans fn in
        Some (fun k -> continue k ())
    | _ -> None
  in
  match_with fn orphans { retc; exnc; effc }

module Make (Flow : Flow.S) (Runtime : S) = struct
  let recv flow buffer =
    let bytes_read =
      Buffer.put buffer ~f:(fun bstr ~off:dst_off ~len ->
          let buf = Bytes.create len in
          match Flow.read flow buf ~off:0 ~len with
          | Ok 0 -> 0
          | Ok len ->
              Bigstringaf.blit_from_bytes buf ~src_off:0 bstr ~dst_off ~len;
              len
          | Error err ->
              Flow.close flow;
              raise (Flow (Fmt.str "%a" Flow.pp_error err)))
    in
    if bytes_read = 0 then `Eof else `Ok bytes_read

  let writev flow bstrs =
    let copy { Faraday.buffer; off; len } = Bigstringaf.copy buffer ~off ~len in
    let css = List.map copy bstrs |> List.map Cstruct.of_bigarray in
    match Flow.writev flow css with
    | Ok () ->
        let len = List.fold_left (fun a { Cstruct.len; _ } -> a + len) 0 css in
        `Ok len
    | Error _ -> Flow.close flow; `Closed

  let run conn ~read_buffer_size flow =
    let buffer = Buffer.create read_buffer_size in
    let rec reader () =
      let rec go orphans =
        match Runtime.next_read_operation conn with
        | `Read ->
            if recv flow buffer = `Eof then
              let _ = Buffer.get buffer ~f:(Runtime.read_eof conn) in
              go orphans
            else
              let _ = Buffer.get buffer ~f:(Runtime.read conn) in
              go orphans
        | `Yield ->
            let k () = Effect.perform (Spawn reader) in
            Runtime.yield_reader conn k;
            terminate orphans
        | `Close -> terminate orphans
      in
      try flat_tasks go with exn -> Runtime.report_exn conn exn
    in
    let rec writer () =
      let rec go orphans =
        match Runtime.next_write_operation conn with
        | `Write iovecs ->
            writev flow iovecs |> Runtime.report_write_result conn;
            go orphans
        | `Yield ->
            let k () = Effect.perform (Spawn writer) in
            Runtime.yield_writer conn k;
            terminate orphans
        | `Close _ -> Flow.shutdown flow `Send; terminate orphans
      in
      try flat_tasks go with exn -> Runtime.report_exn conn exn
    in
    Miou.call_cc @@ fun () ->
    let p0 = Miou.call_cc reader and p1 = Miou.call_cc writer in
    match Miou.await_all [ p0; p1 ] with
    | [ Ok (); Ok () ] -> Ok ()
    | [ Error exn; _ ] | [ _; Error exn ] -> Error exn
    | _ -> assert false
end
