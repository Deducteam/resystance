open Core
open Lplib
open Common.Debug
open Timed
open Term

module U = Unification

let log_cp = new_logger 'k' "krit" "critical pairs"
let log_cp = log_cp.logger

(* It is possible to be more subtle than that, removing refs from
   head symbols should be enough. *)
(** [deep_untref t] removes all references from term [t]. *)
let rec deep_untref : term -> term = fun t ->
  match Term.get_args t with
  | TRef(t), args ->
    begin match !t with
    | None    -> assert false
    | Some(t) -> Term.add_args t (List.map deep_untref args)
    end
  | t      , args -> Term.add_args t (List.map deep_untref args)

(** [unify t u] returns a unifier of [t =? u] or [None]. *)
let unify : term -> term -> U.substitution option = fun t u ->
  let t = deep_untref t in
  let u = deep_untref u in
  try Some(Unification.unify t u)
  with Unification.CantUnify -> None

type path_elt = int
type path = path_elt list
let pp_path_elt = Format.pp_print_int
                  
(** [subterms_of t] returns the subterms of term [t] which are not rewriting
    variables. *)
(*let rec subterms_of : path -> term -> (path * term) list = fun p t ->
  match Term.get_args t with
  | Meta(_, _)   , _ -> []
  | Patt(_, _, _), _ -> []
  | Symb(_, _), args
  | Abst(_, _), args
  | Vari(_)   , args -> (List.rev p,t) :: (List.mapi (fun i a -> subterms_of (i::p) a) args |> List.concat)
  | _ -> assert false
 *)
let rec subterms_of : path -> term -> (path * term) list = fun p t ->
  match unfold t with
  | Meta(_, _)
  | Patt(_, _, _)
  | Symb(_)
  | Vari(_) -> []
  | Appl(t1,t2) -> (List.rev p, t) :: subterms_of (0::p) t1 @ subterms_of (1::p) t2
  | Abst(_, u) ->
     (List.rev p, t) ::
     let (_x,u) = Bindlib.unbind u in
     subterms_of (0::p) u
  | _ -> assert false

(** [apply_to_subterm f p t] applies [f] to the subterm of [t]
    specified by [p] *)
(*let apply_to_subterm f p t =
  let rec apply p t =
    match p with
      [] -> f t
    | i::p ->
       let (s,l) = Term.get_args t in
       (match List.cut l i  with
         (l1,a::l2) -> Term.add_args s (l1@apply p a::l2)
        | _ -> assert false) in
  apply p t*)
let apply_to_subterm f p t =
  let rec apply p t =
    match p, unfold t with
      [],t -> f t
    | 0::p, Appl(t1,t2) -> mk_Appl(apply p t1, t2)
    | 1::p, Appl(t1,t2) -> mk_Appl(t1, apply p t2)
    | 0::p, Abst(t1,u) ->
       let (x,u) = Bindlib.unbind u in
       Bindlib.(unbox(_Abst(box t1)(bind_var x (Term.lift (apply p u)))))
    | _,_ -> assert false in
  apply p t

let apply_rule r lhs =
  assert (Array.for_all (fun n -> n=0) r.arities);
  let e = Array.make (Array.length r.vars) TE_None in
  let rec match_lhs pl tl =
    match pl, tl with
    | (p::pl), (t::tl) ->
       (match Term.get_args p, Term.get_args t with
        | (Patt(Some i,_,_),[]), _ ->
           e.(i) <- TE_Some Bindlib.(unbox(bind_mvar[||](box t)));
           match_lhs pl tl
        | (Patt _, _), _ -> 
           match_lhs pl tl
        | (Symb(_),pal), (Symb(_),al) when List.length pal = List.length al ->
           match_lhs (pal@pl) (al@tl)
        | (Abst(_,p1),pal), (Abst(_,t1),al) when List.length pal = List.length al ->
           let (_,p1,t1) = Bindlib.unbind2 p1 t1 in
           match_lhs (p1::pal@pl) (t1::al@tl)
        | (Vari(_),pal), (Vari(_),al) when List.length pal = List.length al ->
           match_lhs (pal@pl) (al@tl)
        | _ -> assert false)
    | [], [] ->
       Bindlib.msubst r.rhs e
    | _ -> assert false in
  let (_,args) = Term.get_args lhs in
  match_lhs r.lhs args

let rename_patt_var pm vn =
  match Hashtbl.find_opt pm vn with
  | None ->
     let vnuntagged = (*Filename.remove_extension*) vn in
     let v = new_tvar vnuntagged in
     Hashtbl.add pm vn v;
     v
  | Some(n) -> n

let patt_to_var : term -> term = fun te ->
  let pm = Hashtbl.create 997 in
  let rec loop : term -> term = fun te ->
    match unfold te with
    | Appl(t, u) ->
      let t = loop t in
      let u = loop u in
      mk_Appl(t, u)
    | Patt(_, v, [||]) ->
      let name = rename_patt_var pm v in
      mk_Vari name
    | Abst(t1,t2) ->
       let (x,u) = Bindlib.unbind t2 in
       Bindlib.(unbox(_Abst(box (loop t1))(bind_var x (Term.lift (loop u)))))
    | Patt(_, _, _) -> assert false
    | t -> t
  in
  loop te


(** [cps l1 l2] searches for critical peaks involving lhs [l1] and
    subterms of lhs [l2].  A returned quadruple [(l1r, l2r, ls, s)]
    contains
    - [l1r] [l1] renamed;
    - [l2r] [l2] renamed;
    - [ls] [l2] renamed with the unifier [s] applied;
    - [s] the unifier. *)
type lhs = sym * rule
type critical_pair = (path*lhs*term) * (lhs*term) * U.substitution


let reduce ((p,r1,_l1),(r2,l2),s) =
  let rl2 = patt_to_var (U.lift s l2) in
  let rhs1 = apply_to_subterm (apply_rule (snd r1)) p rl2 in
  let rhs2 = apply_rule (snd r2) rl2 in
(*  Format.eprintf "REDUCE:\n %a\n %a\n %a\n"
    Print.pp rl2 Print.pp rhs1 Print.pp rhs2;*)
  (rl2,(p,r1,rhs1),(r2,rhs2))

let cps_sym : lhs * term -> lhs * term -> critical_pair list =
  fun (r1,l1) (r2, l2) ->
(*  if r1 == r2 then [] (* discard refl at toplevel *)
  else*)
    let l1 = U.rename l1 in
    let l2 = U.rename l2 in
    match unify l1 l2 with
    | Some s -> [([],r1,l1),(r2,l2),s]
    | None -> []
            
let cps : lhs * term -> lhs * term -> critical_pair list =
  fun (r1,l1) (r2, l2) ->
  let l1 = U.rename l1 in
  let l2 = U.rename l2 in
  subterms_of [] l2
  |> List.filter (fun (p,_) -> p<>[]) (* pairs at toplevel processed by cps_sym *)
  |> List.filter_map
       (fun (p,t) -> Option.map (fun s-> (p,r1,l1),(r2,l2),s) (unify l1 t))

let rec map_pair_keep_order f l =
  match l with
  | [] -> []
  | x::l -> (List.map (f x) l |> List.flatten) @ map_pair_keep_order f l
  
let cps : sym list -> critical_pair list = fun syms ->
  let rules_of_sym s =
    match !(s.sym_def) with
    | Some d -> [{lhs=[]; rhs=Bindlib.(unbox(bind_mvar[||](box d)));
                  arity=0; arities=[||]; vars=[||]; xvars_nb=0}]
    | None -> !(s.sym_rules) in
  let term_of_lhs s =
    List.to_seq (rules_of_sym s)
    |> Seq.map (fun l -> (s,l), Term.add_args (mk_Symb s) l.lhs)
  in
  let lhs =
    List.to_seq syms |> Seq.flat_map term_of_lhs |> List.of_seq
  in
  let f l1 =
    List.map (fun l2 -> cps l1 l2) lhs |> List.flatten
  in
  let res = map_pair_keep_order cps_sym lhs @
              (List.map f lhs |> List.flatten) in
  if !Common.Debug.log_enabled then
    log_cp "%d symbols, %d rules, %d pairs\n"
      (List.length syms) (List.length lhs) (List.length res);
  res
