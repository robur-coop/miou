module Make () = struct
  type t = int

  let null = 0
  let pp = Format.pp_print_int
  let compare a b = a - b
  let equal = Int.equal

  external of_int : int -> t = "%identity"
  external to_int : t -> int = "%identity"

  let gen, reset =
    let value = Atomic.make (succ null) in
    let gen () = Atomic.fetch_and_add value 1 in
    let reset () = Atomic.set value (succ null) in
    (gen, reset)
end
