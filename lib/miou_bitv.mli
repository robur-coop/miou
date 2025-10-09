(** This module implements bit vectors, as an abstract datatype [t]. Since bit
    vectors are particular cases of arrays, this module provides the same
    operations as module [Array]. It also provides bitwise operations and
    conversions to/from integer types.

    In the following, [false] stands for bit 0 and [true] for bit 1. *)

type t
(** The type of bit vectors *)

val create : int -> bool -> t
(** [create n b] creates a new bit vector of length [n], initialized with [b].
*)

val length : t -> int
(** [length] returns the length (number of elements) of the given vector. *)

val next : t -> int option
(** [next v] returns the next unset [n]th bit of [v]. *)

val max : t -> int

val get : t -> int -> bool
(** [get v n] returns the [n]th bit of [v]. *)

val set : t -> int -> bool -> unit
(** [set v n b] sets the [n]th bit of [v] to the value [b]. *)

val iter : (int -> unit) -> t -> unit
(** [iter fn v] applies function [fn] in turn to all indexes of the elements of
    [v] which are set (i.e. [true]); indexes are visited from least significant
    to most significant. *)

val equal : t -> t -> bool
(** Returns [true] if two bit vectors are of the same length and with the same
    bits. *)
