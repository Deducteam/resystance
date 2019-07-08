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
  ; sd : float
  (** Standard deviation. *)}
  [@@deriving yojson]

(** [of_list l] creates a distribution from a list. *)
let of_list : int list -> t = fun l -> { values = l ; cardinal = List.length l }

(** [init] creates an empty distribution with value [x]. *)
let empty : t =
  { cardinal = 0 ; values = [] }

(** [merge t u] merges two distributions [t] and [u] into one. *)
let merge : t -> t -> t = fun a b ->
  { cardinal = a.cardinal + b.cardinal
  ; values = List.sort compare (a.values @ b.values) }

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
  let sd = sqrt @@ sqavg -. avg *. avg in
  { percentiles ; average = avg ; sd }
