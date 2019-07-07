(** Manipulation statistical distributions. *)

(** Type of a distribution of values of type ['a]. *)
type 'a t =
  { percentile_25 : 'a
  (** 25th percentile. *)
  ; median : 'a
  (** Median or 50th percentile. *)
  ; average : 'a
  (** Average *)
  ; percentile_75 : 'a
  (** 75th percentile. *) }
  [@@deriving yojson]

(** [init x] creates an empty distribution with value [x]. *)
let init : 'a -> 'a t = fun x ->
  { percentile_25 = x ; median = x ; average = x ; percentile_75 = x }
