(* On essaye de mettre en place un algorithme donnant un ensemble de regles de reecriture  avec en argument une theorie equationnelle AC.

On suppose pour l'instant que l'on a un ordre sur les termes et qu'on a un algorithme donnant la forme normale.

On definit une theorie equationnelle comme un ensemble de couples. Si A+B, on a (a,b) et (b,a). *)


(*FAIT*)

(* il faut implementer une fonction donnant un des plus petit element et le retire si possible: smallest ()  FAIT *)
(* il faut une fonction qui retire (a,b) et (b,a) de l'ensemble des equations : suppr inclu dans smallest() FAIT*)
(*Il faut une fonction qui verifie que (a,bo) est dans eqs: appartient() FAIT *)
(*On va devoir implementer une fonction permettant de retirer une regle de Regles *)
(*On suppose qu'on a une fonction retire A B qui donne l'ensemble A\B *)


(*A Faire *)

(*Il faut mettre une fonction qui puisse transformer les variables en pattern et  *)
(*On doit ecrire une fonction qui transforme une regle en son extension*)
(*On a besoin d'une fonction qui à un terme associe toutes ses variables *)
(*On va supposer qu'on a dejà des ensembles AC, A et C *)
(*On suppose qu'il existe une fonction qui à une regle donne le sym du lhs *)
(*Modifier le dtree *)

open Core
open Term
open Timed
open Sign
open Parsing.Scope
open Parsing
open Syntax
open Tool
open Sr
open Common
open Pos

module U = Unification



let sig_state = Sig_state.dummy;;
let superieur a b=a>b;;
let ac = [] ;;

let rec to_pterm t= match unfold t with
    |Appl(t,u) -> P_Appl(Pos.none (to_pterm t), Pos.none(to_pterm u))
    |Vari x -> P_Iden(Pos.none([], Bindlib.name_of x), false)
    |Abst (t,u) -> let (x, u_) = Bindlib.unbind u in P_Abst([[Some (Pos.none(Bindlib.name_of x))], Some(Pos.none(to_pterm t)), false], Pos.none(to_pterm u_))
    |Prod (t,u) -> let (x, u_) = Bindlib.unbind u in P_Prod([[Some (Pos.none(Bindlib.name_of x))], Some(Pos.none(to_pterm t)), false], Pos.none(to_pterm u_))
    |Symb s ->  P_Iden (Pos.none (s.sym_path, s.sym_name), false);;

let new_rule lhs rhs =
    let p_lhs = to_pterm lhs in
    let p_rhs = to_pterm rhs in
    Sr.check_rule (Scope.scope_rule false sig_state (Pos.none ((Pos.none (p_lhs),Pos.none (p_rhs)))));;


(**On suppose qu'on a en entrée sign la signature*)
let rec choisit k liste = match liste with
    |t::q when k==0 -> t
    |t::q -> choisit (k-1) q;;



(**l     fonction sélectionne le couple le plus petit et le retire, un potentiel ordre est cmp dans Term*)
let smallest liste =
    let rec smallestrec liste (a,b) = match liste with
        |(t,u)::[]->if ((Term.cmp a t <0) || ((Term.cmp a t =0) && (Term.cmp b u <0)) )then (a,b) else (t,u)
        |(t,u)::q when ((Term.cmp a t <0) || ((Term.cmp a t =0) && (Term.cmp b t <0))) -> smallestrec q (a,b)
        |t::q -> smallestrec q t in 
        smallestrec liste (choisit 0 liste);;

(**Cette fonction retire une faire d'une liste*)
let retire liste triplet =
    let rec retire liste (sym,a,b) = match liste with
            |(s,c,d)::q when ((Term.cmp s sym = 0) && (Term.cmp c a = 0) && (Term.cmp d b=0)) || ((Term.cmp s sym=0) && (Term.cmp c b =0)&& (Term.cmp d a=0))->retire q (sym,a,b)
            |[]->[]
            |(s,c,d)::q -> (s,c,d)::retire q (sym,a,b) in retire liste triplet;;

let retire2 liste paire =
    let rec retire liste (a,b) = match liste with
        |(c,d)::q when ( (Term.cmp c a=0) && (Term.cmp d b=0)) || ( (Term.cmp c b =0)&& (Term.cmp d a=0))->retire q (a,b)
        |[]->[]
        |(c,d)::q -> (c,d)::retire q (a,b) in retire liste paire;;

(**On verifie qu'un élément est bien dans un ensemble donné*)
let appartient liste (sym,a,b,rule)= 
    let rec appartientrec liste (sym,a,b, rule)= match liste with 
        |[]-> false
        |(_,t,u,_)::q when ((Term.cmp t a=0) && (Term.cmp u b =0)) -> true
        |t::q -> appartientrec q (sym,a,b,rule) in appartientrec liste (sym,a,b,rule);;


let appartientens liste (a,b)= 
    let rec appartientrec liste (a,b)= match liste with 
        |[]-> false
        |(t,u)::q when ((Term.cmp t a=0) && (Term.cmp u b =0)) -> true
        |t::q -> appartientrec q (a,b) in appartientrec liste (a,b);;

(*On calcule A\B*)
let complementaire listea listeb= 
    let rec complementairerec listea listeb = match listea with 
        |[]-> []
        |(a,b,t,u)::q when appartient listeb (a,b,t,u)-> complementairerec q listeb
        |t::q -> t::complementairerec q listeb in complementairerec listea listeb;;

(**Cette fonction permet de calculer l'extension AC d'une liste de règles*)

let  extension_AC regle = 
    let rec extension_aux_rec sym regle liste = match regle with
            |[] -> liste
            |(s, a,b,t)::v when (Term.cmp (mk_Symb s) sym=0)-> let var = of_tvar (new_tvar "var") in extension_aux_rec sym v ((s, mk_Appl(mk_Appl(sym, var),a),mk_Appl(mk_Appl(sym,var), b),t)::liste) in 


    let rec extension_rec ac regle liste =match ac with 
        |[]-> liste
        |t::q -> let l =extension_aux_rec t regle liste in extension_rec q regle l in

        extension_rec ac regle [];; 

(**Cette fonction permet de calculer l'extension A d'une liste de règles*)

let  extension_A regle = 
    let rec extension_aux_rec sym regle liste = match regle with
            |[] -> liste
            |(s,a,b,t)::v when (Term.cmp (mk_Symb s) sym=0)-> let var = of_tvar (new_tvar "var") in let var2 = of_tvar (new_tvar "var2") in extension_aux_rec sym v ((s, mk_Appl(mk_Appl(sym, var), mk_Appl(mk_Appl(sym, a),var2)), mk_Appl(mk_Appl(sym, var), mk_Appl(mk_Appl(sym,b), var2)),t)::(s, mk_Appl(mk_Appl(sym, a),var ),mk_Appl(mk_Appl(sym,b), var),t)::(s, mk_Appl(mk_Appl(sym, var),a ),mk_Appl(mk_Appl(sym,var), b),t)::liste) in 

    let rec extension_rec associatif regle liste =match associatif with 
        |[]-> liste
        |t::q -> let l =extension_aux_rec t regle liste in extension_rec q regle l in

        extension_rec ac regle [];; 

        
(**On calcule ici l'ensemble extension de règle*)
let extension regle = (extension_A regle )@(extension_AC regle ) @ regle;;


(**On fait la première étape de calucl au sein de pair*)
let pair11 pairs regles regle extensions= 
    let rec aux1 liste1 regle = match liste1 with
        |t::[]->[(t,regle)]
        |t::q->(t, regle)::(aux1 q regle) in
    let rec aux1_ liste1 regle = match liste1 with
        |t::[]->[(regle,t)]
        |t::q->(regle, t)::(aux1_ q regle) in
    let rec aux2 liste1 liste2 = match liste2 with
        |t::[]-> aux1 liste1 t
        |t::q-> (aux1 liste1 t)@(aux2 liste1 q)in
    let rec pairs_rec pairs regles extensions= match regles with 
        |[]-> (pairs, extensions)
        |(s, a,b,t)::q-> let new_pair = aux1 (extension [regle]) (s,a,b,t) @ aux1_ (extension [(s,a,b,t)]) regle @ aux2 (extension [regle]) (extension [(s,a,b,t)]) in pairs_rec (pairs@[((s,a,b,t),regle)]@new_pair) q extensions in 
        pairs_rec pairs regles ((extension [regle])@(extension regles)@extensions);;

let appartientpairs liste ((a,b,c,d),(a_,b_,c_,d_))= 
    let rec appartientrec liste ((a,b,c,d),(a_,b_,c_,d_))= match liste with 
        |[]-> false
        |((_,t,u,_),(_,v,w,_))::q when ((Term.cmp t b=0) && (Term.cmp u c =0)) && (Term.cmp v b_=0) && (Term.cmp w c_ =0) -> true
        |t::q -> appartientrec q ((a,b,c,d),(a_,b_,c_,d_)) in appartientrec liste ((a,b,c,d),(a_,b_,c_,d_));;

(*On calcule A\B*)
let complementairepairs listea listeb= 
    let rec complementairerec listea listeb = match listea with 
        |[]-> []
        |((v,t,u,w),(v_,t_,u_,w_))::q when appartientpairs listeb ((v,t,u,w),(v_,t_,u_,w_))-> complementairerec q listeb
        |t::q -> t::complementairerec q listeb in complementairerec listea listeb;;

let retire3 liste paire =
    let rec retire liste ((i,t,u,h),(w,x,y,z)) = match liste with
        |((b,c,d,a), (b_,c_,d_,a_))::q when ( (Term.cmp c t=0) && (Term.cmp d u=0)) && ( (Term.cmp c_ x =0)&& (Term.cmp d_ y=0))->retire q ((i,t,u,h),(w,x,y,z))
        |[]->[]
        |((b,c,d,a), (b_,c_,d_,a_))::q -> ((b,c,d,a), (b_,c_,d_,a_))::retire q ((i,t,u,h),(w,x,y,z)) in retire liste paire;;







(**On fait la seconde étape de calcul de pair*)      
let pair17 pairs regles regle extensions = 
    let rec aux1 liste1 regle = match liste1 with
        |t::[]->[(t,regle)]
        |t::q->(t, regle)::(aux1 q regle) in
    let rec aux1_ liste1 regle = match liste1 with
        |t::[]->[(regle,t)]
        |t::q->(regle, t)::(aux1_ q regle) in
    let rec aux2 liste1 liste2 = match liste2 with
        |t::[]-> aux1 liste1 t
        |t::q-> (aux1 liste1 t)@(aux2 liste1 q)in
    let rec pairs_rec pairs regles extensions= match regles with 
        |[]-> pairs, extensions
        |(s,a,b,t)::q-> let new_pair = complementairepairs pairs ([regle, (s,a,b,t)]@aux1 (extension [regle]) (s,a,b,t) @ aux1_ (extension [(s,a,b,t)]) regle @ aux2 (extension [regle]) (extension [(s,a,b,t)])) in pairs_rec new_pair q extensions in 
        pairs_rec pairs regles ((extension [regle])@(extension regles)@extensions);;


let smallestpair liste =
    let rec smallestrec liste ((b,c,d,a), (b_,c_,d_,a_))= match liste with
        |((i,t,u,h),(w,x,y,z))::[] when (((Term.cmp c t <0) || ((Term.cmp c t =0) && (Term.cmp d u <=0)) )) -> ((b,c,d,a), (b_,c_,d_,a_))
        |((i,t,u,h),(w,x,y,z))::[] when (((Term.cmp c t =0) && (Term.cmp d u =0)) && ((Term.cmp c_ x >0) || ((Term.cmp c_ x =0) && (Term.cmp d_ y >0)) )) -> ((i,t,u,h),(w,x,y,z))
        |((i,t,u,h),(w,x,y,z))::[] when ((Term.cmp c t >0) || ((Term.cmp c t =0) && (Term.cmp d u >0)) ) -> ((i,t,u,h),(w,x,y,z))
        |((i,t,u,h),(w,x,y,z))::q when (((Term.cmp c t <0) || ((Term.cmp c t =0) && (Term.cmp d u <=0)) ))-> smallestrec q ((b,c,d,a), (b_,c_,d_,a_))
        |((i,t,u,h),(w,x,y,z))::q when (((Term.cmp c t =0) && (Term.cmp d u =0)) && ((Term.cmp c_ x >0) || ((Term.cmp c_ x =0) && (Term.cmp d_ y >0)) )) -> smallestrec q ((i,t,u,h),(w,x,y,z))
        |((i,t,u,h),(w,x,y,z))::q when ((Term.cmp c t >0) || ((Term.cmp c t =0) && (Term.cmp d u >0)) ) -> smallestrec q ((i,t,u,h),(w,x,y,z)) in 
        smallestrec liste (choisit 0 liste);;


let rec extrait a liste =match liste with
    |t::q when  t == a-> extrait a q (**Attention, ici c'est du cmp sur un type rule pas Term ca ne marche peut etre pas*)
    |t::q -> t::extrait a q
    |[]->[];;



let del_rule :  sym -> rule -> unit = fun  sym r ->
  sym.sym_rules := extrait r (!(sym.sym_rules)); Tree.update_dtree sym [];;

let del regle regles =
    let (sym, lhs, rhs, rule )= regle in
    let regles = extrait regle regles in
    del_rule sym rule; Tree.update_dtree sym []; regles;;


let ajoute regle regles=
    let (sym, lhs, rhs )= regle in
    let rule = new_rule lhs rhs  in
    let regles = regles @ [(sym,lhs,rhs,rule)] in
    Sign.add_rule sig_state.signature sym rule; Tree.update_dtree sym []; regles;;
    

let rec dernier_elem l=match l with
    |t::[] -> t
    |t::q -> dernier_elem q
    |[]-> failwith "liste vide";;



let new_liste n=
    let rec new_liste_rec n = match n with
        |0 -> []
        |t-> 0::new_liste_rec (t-1)in
    new_liste_rec n;;

let modif l n= 
    let rec modif_rec l n = match n, l with
        |(0, t::q)-> 1::q
        |(n, t::q)-> t::modif_rec q (n-1)in
    modif_rec l n;;

type path_elt = int
type path = path_elt list


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
  | _ -> assert false;;

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
  apply p t;;

let fonction1 (lambda1, rho1) (lambda2, rho2) = 
    let sigma = Unification.unify lambda1 lambda2 in
    [((Unification.lift sigma rho1 ), (Unification.lift sigma rho2 ))];; (**la fonction lift applique une substitution à un terme *)

let retire_premier l= match l with (**retire le premier terme d'une liste*)
    |t::q -> q
    |[]-> [];;

let rec selectionne k l =match k, l with
    |(1, t::q) -> t
    |(n, t::q)-> selectionne (n-1) q;;


let fonction2 (lambda1, rho1) (lambda2, rho2) = (*attention à ne pas prendre le sous terme corespond au terme complet*) 
    let sous_terms= retire_premier (subterms_of [] lambda1) in 
    let n = List.length sous_terms in
    let liste = ref [] in 
    for indent=1 to n do 
        let sous_term = selectionne n sous_terms in 
        let _,term = sous_term in
        let sigma = Unification.unify term lambda2 in 
        let f t = Unification.lift sigma t in
        let path,term = sous_term in
        liste:= ((Unification.lift sigma rho1 ), (apply_to_subterm f path term) )  ::!liste
    done;
    !liste;;

    (**garder la généralité de apply_to_subterm*)

let rec appartient_fin (lambda, rho) liste= match liste with
        |(_,a,b,_)::q when ((Term.cmp a lambda=0)&& (Term.cmp b rho)=0)-> true
        |(_,a,b,_)::q -> appartient_fin (lambda,rho) q
        |[] -> false;;




let fonction_finale (lambda1, rho1) (lambda2, rho2) extensions=
    if appartient_fin (lambda1, rho1) extensions then 
        if (appartient_fin (lambda2, rho2) extensions) then
            fonction1 (lambda1, rho1) (lambda2, rho2)
        else (fonction1 (lambda1, rho1) (lambda2, rho2))@(fonction2 (lambda2, rho2) (lambda1, rho1))
    else
        if (appartient_fin (lambda2, rho2) extensions) then
            fonction1 (lambda1, rho1) (lambda2, rho2)@(fonction2 (lambda1, rho1) (lambda2, rho2) )
        else (fonction1 (lambda1, rho1) (lambda2, rho2))@(fonction2 (lambda1, rho1) (lambda2, rho2) )@(fonction2 (lambda2, rho2) (lambda1, rho1));;


    
(**fonction correspondant à l'algorithme complet*)
let  algo eqs regles pairs = 
let rec algorec eqs regles  pairs extensions=
if eqs<>[] then 
    let (s,t)=smallest eqs in 
    let (s_,t_) = (Eval.snf [] s , Eval.snf [] t ) in (**modifiactions du 23/03, les règles sont incluse dans l'ensemble. Il ne fut pas les rappeler *)
    let eqs = retire2 eqs (s,t)  in
    if appartientens eqs (s_,t_) (**faire attention car c'est une égalité au sein de T normalement*) then algorec eqs regles pairs extensions 
    else
        let rec intermediaire s_ t_ regles pairs extensions= (**Eq modulo ou la comparaison de deux termes  dans Terms*)
        if Eval.eq_modulo [] s_ t_ then algorec eqs regles pairs extensions 
        else 
            let lambda, rho = if (Term.cmp s_ t_>0) then (s_ ,t_) else  (t_, s_)  in
            
            let (sym, lhs)=get_args lambda in
            let sym = match unfold sym with
                |Symb(s)-> s
                |_ -> failwith "sym n'est pas un symbole" in
            let regles = ajoute (sym, lambda, rho) regles in

            let pairs, extensions = pair11  pairs regles (dernier_elem regles) extensions in 

            let k=ref (List.length regles) in
            let a=ref 1 in

            let rec intermediaire2 regles pairs extensions a k= if !a > !k then algorec eqs regles  pairs extensions else
                let r= choisit (!a-1) regles in 
                let sym,s,t, rule= r in 

                let regles = del r regles in
            
                let s_ = Eval.snf [] s in
                let t_ = Eval.snf [] t in
                (**On doit réinsérer la règle retiré plus haut à ce moment là*)
                
                let regles = ajoute (sym, s, t) regles in

                if (Term.cmp s s_ =0) && (Term.cmp t t_ =0) then 
                    (a:= (!a+1) ; intermediaire2 regles pairs extensions a k)
                else 
                begin
                if (appartientens  eqs (s_, t_)) && (appartientens eqs (t_, s_)) (**attention car égalité dans T*) then 
                    (let pairs, extensions = pair17 pairs regles r extensions in
                    let r= dernier_elem regles in
                    let regles = del r regles in
                    k:=(!k-1);
                    intermediaire2 regles pairs extensions a k)
                else
                    let pairs, extensions = pair17 pairs regles r extensions in
                    let r= dernier_elem regles in
                    let regles = del r regles in
                    intermediaire s_ t_ regles pairs extensions
                end in 
            intermediaire2 regles pairs extensions a k
        in
        intermediaire s_ t_ regles pairs extensions
else
        if pairs<>[] then
            begin
            let ((sym1,lambda1, rho1,rule1), (sym2,lambda2, rho2,rule2)) = smallestpair pairs in
            let pairs =retire3  pairs ((sym1, lambda1, rho1,rule1), (sym2, lambda2, rho2,rule2))  in
            let eqs = fonction_finale ( lambda1, rho1) (lambda2, rho2) extensions in 
            algorec eqs regles pairs extensions
            end
        else regles in
algorec eqs regles pairs [];;




(*On va calculer des paire critiques


Pour déterminer les unifications, on pourrait utiliser la fonction unify de unification (unicité ?)

il faut pour les autres parties déterminer des paires critiques entre les deux règles *)



    (*Finir cette fonction qui fait la 2eme partie de la fin, on doit tout parcourir*)





(**Il existe une fonction Tree.update_dtree dans tree qui prend une règle et qui update son dtree ==> pratique, en plus, elle utilise la fonction lazy qui permet d'appliquer la fonction que quand on a besoin de son résultat. Si ce 'est jamais le cas, on ne l'applique jamais)
Il existe également une fonction add_rule au sein de sign.ml mais il faut un symbole de signature, il faut regarder comment générer ce symbole en regardant des exemples d'utilisation
On peut voir différent exemple dans test/rewriting.ml ou handle/command.ml

Qu'est qu'une configuration==> il y a au sein de eval.ml un tel type qui est définit. Lorsqu'on cherche à mettre sous forme normale, si on cehrche à vérifier une égalité, il peut etre plus efficace de faire par réccurence élément de tête par élément de tête.
Dans le cas d'une inégalité, il faut potentiellement mieux chercher la forme snf

Il faut regarder eq_modulo qui fait étape par étape, on pourrait essayre de faire pareil pour la comparaison d'égalité*)

(**On va commencer par regarder comment comparer avec des inégalités : En réalité ce n'eest pas utile ici



Pour le symbole de signature, il faut utiliser Sig_state, mais il faudrait essayer de comprendre comment il fonctionne (en fait on suppose que la signature est une donnée du problème
*


Il y a un fichier Sig_state dans core
*)