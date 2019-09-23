(** Holds data and statistics. *)
type t

(** Empty dataset *)
val empty : t

(** [of_sig s] computes statistics from signature [s]. *)
val of_sig : Core.Sign.t -> t

(** [merge d e] merges datasets [d] and [e] into one. *)
val merge : t -> t -> t

(** [pp f d] pretty prints data [d] to formatter [f]. *)
val pp : Format.formatter -> t -> unit

(** Header of the csv file. *)
val csv_hdr : string

(** [pp_csv f d] outputs a subset of the information in a dataset as
    csv. *)
val pp_csv : Format.formatter -> t -> unit
