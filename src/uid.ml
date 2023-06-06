type t = int

let of_int x = x
let to_int x = x

let gen, reset =
  let v = Atomic.make 0 in
  let gen () = Atomic.fetch_and_add v 1 and reset () = Atomic.set v 0 in
  (gen, reset)

let parallel = gen
let pp = Format.pp_print_int

type _ Effect.t += Uid : t Effect.t

let concurrent () =
  try Effect.perform Uid
  with Effect.Unhandled effect -> raise Base.(Outside (Effect effect))
