(* NOTE(dinosaure): This test show an other implementation of /boxes/ which does
   not work when the cancellation is used. *)

module Box = struct
  type 'a t = { mutable value: 'a option; lock: Mutex.t * Condition.t }

  let push t value =
    Mutex.lock (fst t.lock);
    t.value <- Some value;
    Condition.broadcast (snd t.lock);
    Mutex.unlock (fst t.lock)

  let take t =
    Mutex.lock (fst t.lock);
    while Option.is_none t.value do
      Condition.wait (snd t.lock) (fst t.lock)
    done;
    let value = Option.get t.value in
    Mutex.unlock (fst t.lock);
    value

  let make () = { value= None; lock= (Mutex.create (), Condition.create ()) }
end

let prgm () =
  let box = Box.make () in
  let perform = function
    | `Push (box, v) -> Box.push box v
    | `Wait -> ignore (Box.take box)
  in
  Miou.parallel perform [ `Push (box, ()); `Wait; `Wait ]
  |> List.iter @@ function Error exn -> raise exn | Ok () -> ()

let () = Miouu.run ~domains:3 prgm
