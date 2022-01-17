(** Syntactic unification operating on Dk3 terms.

    The variables of the unification are rewriting variables, also called
    pattern variables, built with {!constructor:Core.Term.Patt}. *)

open Core

(** {2 Variables} *)

val rename : Term.term -> Term.term
(** [rename t] returns term [t] with its (rewrite) variables with
    fresh names (non linearity is kept). *)

(** {2 Substitutions} *)

type substitution
(** Substitutions from pattern variables to terms. *)

val pp_subst : Format.formatter -> substitution -> unit
(** [pp_subst f s] pretty prints substitution [s] to formatter [f]. *)

val lift : substitution -> Term.term -> Term.term
(** [lift s t] applies substitution [s] to term [t]. *)

(*val lz_lift : substitution -> Term.term -> Terms.term*)
(** [lift s t] applies substitution [s] to term [t] (recursively!). *)

(** {2 Unification} *)

exception CantUnify
(** Raised in case of unification failure. *)

val unify : Term.term -> Term.term -> substitution
(** [unify t u] returns a substitution [s] such that [ts = us].

    @raise CantUnify if [t] and [u] are not unifyable. *)

