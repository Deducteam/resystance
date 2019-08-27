(** Manipulation statistical distributions. *)

(** Type of a distribution of integers. *)
type t = int array

(** [distribution d] sorts dataset [d]. *)
let distribution : t -> t = fun d ->
  Array.fast_sort (-) d ;
  d

(** [of_list l] creates a distribution from a list. *)
let of_list : int list -> t = fun l ->
  distribution @@ Array.of_list l

(** [init] creates an empty distribution with value [x]. *)
let empty : t = [||]

(** [merge t u] merges two distributions [t] and [u] into one. *)
let merge : t -> t -> t = fun a b ->
  distribution (Array.append a b)

let percentile : int -> t -> int = fun k d ->
  if d = [||] then 0 else
  let card = Array.length d in
  let index = int_of_float ((float_of_int (k * card)) /. 100.) in
  d.(index)

let average : t -> float = fun d ->
  (float_of_int (Array.fold_left (+) 0 d)) /. (float_of_int (Array.length d))

(** [sd d] computes the standard deviation of [d]. *)
let sd : t -> float = fun d ->
  let avg = average d in
  let sqavg = average (Array.map (fun x -> x * x) d) in
  sqrt @@ sqavg -. avg *. avg
