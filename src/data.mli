(** Holds data and statistics. *)
type t

(** [of_file f] computes statistics from file [f]. *)
val of_file : string -> t

(** [merge d] merges several datasets into one. *)
val merge : t list -> t

(** [pp f d] pretty prints data [d] to formatter [f]. *)
val pp : Format.formatter -> t -> unit

(** [pp_csv f d] outputs a subset of the information in a dataset as
    csv. *)
val pp_csv : Format.formatter -> t -> unit
