(** Find critical pairs of a system. *)

open Core
open Lplib.Base
open Terms

val log_cp : 'a Console.outfmt -> 'a
   
type path_elt = int
type path = path_elt list
val pp_path_elt : path_elt pp

type lhs = sym * rule
type critical_pair = (path*lhs*term) * (lhs*term) * Unification.substitution

val cps : sym list -> critical_pair list
(** [cps syms] returns a list of critical pairs emerging from rewrite rules on
    symbols [syms]. *)

val reduce : critical_pair -> term * (path * lhs * term) * (lhs *term)
(** [reduce r] transforms the critical pair into a quadruple
    [(t,(p,r1,t1),(r2,t2))] where [t->t1] by rule [r1] at subterm [p] and
    [t->t2] by rule [r2] at the root. *)
