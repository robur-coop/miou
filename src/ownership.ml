type t = { uid: Uid.t; abnormal_finaliser: unit -> unit }

let own abnormal_finaliser =
  let uid = Uid.concurrent () in
  { uid; abnormal_finaliser }

(*
let bless { uid; _ } =
  try Effect.perform (Eff.Bless (Uid.to_int uid))
  with Effect.Unhandled effect -> raise Base.(Outside (Effect effect))
*)
