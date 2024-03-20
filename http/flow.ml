module type S = sig
  type t
  type error

  val pp_error : error Fmt.t
  val read : t -> bytes -> off:int -> len:int -> (int, error) result
  val writev : t -> Cstruct.t list -> (unit, error) result
  val close : t -> unit
  val shutdown : t -> [ `Recv | `Send ] -> unit
end
