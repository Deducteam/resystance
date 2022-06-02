open Core

(*
Cette fonction permet de traduire un couple (lhs,rhs) en un objet de type rule
ayant comme membre gauche lhs et comme membre droit rhs. Pour assurer cette
traduction, on passe par la création de p_terms.*)

val new_rule : Term.term-> Term.term -> Term.rule

(*Les extensions sont des outils permettant de déterminer les différentes paires
critiques pouvant se former lors de l’ajout d’une nouvelle règle de réécriture avec
les symboles associatifs ou commutatifs. Ces extensions sont calculées grâces aux
fonctions extension_A et extension_AC qui prennent en compte respectivement les symboles associatifs et commutatifs et ceux uniquement associatifs. Enfin, la
fonction extension prend en argument une liste de règles et donne l’extension globale
de celle-ci : {extension_AC(règles)}U{extension_A(règles)}U{règles}*)

val extension_A : Term.sym -> Term.term ->Term.term -> Term.rule -> (Term.sym, Term.term,Term.term, Term.rule) list

val extension_AC : Term.sym* Term.term*Term.term* Term.rule -> (Term.sym, Term.term,Term.term, Term.rule) list

(*L’algorithme s’appuie sur le remplissage itératif d’un ensemble nommé Pairs.
Ce remplissage se fait en suivant certaines règles. Ces étapes sont automatisées à
travers les fonctions pair11 et pair17. Elles prennent en entrée l’état initial de la
liste Pairs, celui de la liste Règles, une règle de réécriture et l’ensemble des extensions
correspondant à tous les calculs d’extensions déjà réalisés. Elles renvoient alors la
nouvelle liste Pairs et le nouvel ensemble d’extensions.*)

val pair11 : ((Term.sym* Term.term*Term.term* Term.rule),(Term.sym* Term.term*Term.term* Term.rule)) list -> (Term.sym, Term.term,Term.term, Term.rule) list -> (Term.sym* Term.term*Term.term* Term.rule) -> (Term.sym, Term.term,Term.term, Term.rule) list -> ((Term.sym* Term.term*Term.term* Term.rule),(Term.sym* Term.term*Term.term* Term.rule)) list

val pair17 : ((Term.sym, Term.term,Term.term, Term.rule),(Term.sym, Term.term,Term.term, Term.rule)) list -> (Term.sym, Term.term,Term.term, Term.rule) list -> (Term.sym, Term.term,Term.term, Term.rule) -> (Term.sym, Term.term,Term.term, Term.rule) list -> ((Term.sym, Term.term,Term.term, Term.rule),(Term.sym, Term.term,Term.term, Term.rule)) list


(*Fonctions permettent d’appliquer les étapes 21 à 24 de l’algorithme sur le
traitement des couples de règles au sein de Pairs. Les fonctions fonction1 et fonction2
permettent le traitement de l’étape 24. La fonction fonction_finale permet de créer
le nouvel ensemble Ens d’équations sur lequel on réapplique l’algorithme. Au cours
de ce traitement, on observe la formation de paires critiques entre les différentes
règles. Chaque paire critique est alors convertie en une nouvelle équation réintroduite
ensuite dans l’algorithme.*)


val fonction1 : ((Term.sym, Term.term,Term.term, Term.rule),(Term.sym, Term.term,Term.term, Term.rule))-> ((Term.sym, Term.term,Term.term, Term.rule),(Term.sym, Term.term,Term.term, Term.rule))-> (Term.term,Term.term) list
val fonction2 : ((Term.sym, Term.term,Term.term, Term.rule),(Term.sym, Term.term,Term.term, Term.rule))-> ((Term.sym, Term.term,Term.term, Term.rule),(Term.sym, Term.term,Term.term, Term.rule))-> (Term.term,Term.term) list
val fonction_finale : ((Term.sym, Term.term,Term.term, Term.rule),(Term.sym, Term.term,Term.term, Term.rule))-> ((Term.sym, Term.term,Term.term, Term.rule),(Term.sym, Term.term,Term.term, Term.rule))-> val fonction1 : ((Term.sym, Term.term,Term.term, Term.rule),(Term.sym, Term.term,Term.term, Term.rule))-> ((Term.sym, Term.term,Term.term, Term.rule),(Term.sym, Term.term,Term.term, Term.rule))-> (Term.term,Term.term) list
list -> (Term.term,Term.term) list

(*Cette fonction crée l’algorithme en s’appuyant sur les différentes fonctions créées
en amont. Une description est donnée étape par étape par Peterson.*)

val algo : (Term.term, Term.term)-> (Term.sym, Term.term,Term.term, Term.rule) list -> ((Term.sym, Term.term,Term.term, Term.rule),(Term.sym, Term.term,Term.term, Term.rule)) list -> (Term.sym, Term.term,Term.term, Term.rule) list
