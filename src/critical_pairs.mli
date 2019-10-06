(** Find critical pairs of a system. *)

open Core
open Terms

val cps : Sign.t -> (term * term * term * Unification.substitution) list
(** [cps s] returns a list of critical pairs emerging from rewrite rules of
    signature [s]. *)
