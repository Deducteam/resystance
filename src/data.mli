(** Holds data and statistics. *)
type t

(** [of_file f] computes statistics from file [f]. *)
val of_file : string -> t

(** [merge d] merges several datasets into one. *)
val merge : t list -> t

(** [pp f d] pretty prints data [d] to formatter [f]. *)
val pp : Format.formatter -> t -> unit
