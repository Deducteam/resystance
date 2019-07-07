(** Manipulation statistical distributions. *)

(** Type of a distribution of integers. *)
type t =
  { cardinal : int
  (** Number of values in this distribution. *)
  ; values : int list
  (** Values of the distribution. *) }

(** Values computed from a distribution. *)
type aggregate =
  { percentiles : int array
                      (** Percentiles from 0 to 100. *)
  ; average : float
  ; sd : float }
  [@@deriving yojson]

(** [init x] creates an empty distribution with value [x]. *)
let init : t =
  { cardinal = 0 ; values = [] }

let percentile : int -> t -> int = fun k { cardinal ; values ; _ } ->
  if cardinal = 0 then 0 else
  let index = int_of_float ((float_of_int (k * cardinal)) /. 100.) in
  List.nth values index

let average : t -> float = fun d ->
  (float_of_int (List.fold_left (+) 0 d.values)) /. (float_of_int d.cardinal)

(** [compute d] computes statistics on distribution [d]. *)
let compute : t -> aggregate = fun ({ values ; _ } as v) ->
  let percentiles = Array.init 100 (fun k -> percentile k v) in
  let avg = average v in
  let sqavg = average { v with values = List.map (fun x -> x * x) values } in
  let sd = avg *. avg -. sqavg in
  { percentiles ; average = avg ; sd }
