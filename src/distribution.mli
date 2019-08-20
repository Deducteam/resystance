(** A small library to manipulate distributions of integer. *)

type t
(** Type of a distribution. *)

val empty : t
(** The empty distribution *)

val of_list : int list -> t
(** [of_list l] creates a distribution from a list of integers. *)

val merge : t -> t -> t
(** [merge d e] merges distributions [d] and [e] into one. *)

val percentile : int -> t -> int
(** [percentile k d] returns the [k]th percentile of distribution
 ** [d]. *)

val average : t -> float
(** [average d] returns the average value of distribution [d]. *)

val sd : t -> float
(** [sd d] returns the standard deviation of distribution [d]. *)
