(** Find critical pairs of a system. *)

open Core
open Terms

val cps : sym list -> (term * term * term * Unification.substitution) list
(** [cps syms] returns a list of critical pairs emerging from rewrite rules on
    symbols [syms]. *)
