type error = Picos_exn_bt.t

let to_error (exn, bt) = { Picos_exn_bt.exn; bt }

module Trigger = struct
  include Picos.Trigger

  let[@alert "-handler"] on_signal = on_signal

  let await trigger =
    match await trigger with
    | Some { Picos_exn_bt.exn; bt } -> Some (exn, bt)
    | None as none -> none
end

module Computation = struct
  include Picos.Computation

  let cancelled t =
    match canceled t with
    | Some { Picos_exn_bt.exn; bt } -> Some (exn, bt)
    | None as none -> none

  let await_exn t = await t
  let canceller = canceler
  let create () = create ?mode:None ()
  let try_cancel t (exn, bt) = try_cancel t { Picos_exn_bt.exn; bt }
  let raise_if_errored t = check t

  let peek t =
    match peek t with
    | None as none -> none
    | Some (Ok _) as some -> some
    | Some (Error { Picos_exn_bt.exn; bt }) -> Some (Error (exn, bt))

  let await t =
    match await t with
    | value -> Ok value
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        Error (exn, bt)
end

module Fiber = struct
  type 'a computation = 'a Computation.t

  include Picos.Fiber

  let raise_if_errored t = check t
end
