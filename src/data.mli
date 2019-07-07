(** Holds data and statistics. *)
type t

(** The empty dataset. *)
val empty : t

(** [of_file f] computes statistics from file [f]. *)
val of_file : string -> t

(** [pp f d] pretty prints data [d] to formatter [f]. *)
val pp : Format.formatter -> t -> unit
