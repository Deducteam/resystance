// Taken from "Term rewriting and all that", example 6.2.2 page 139

set debug +ers

symbol const A : TYPE
symbol const e : A

symbol i : A ⇒ A

symbol f : A ⇒ A ⇒ A
rule f (f &x &y) &z → f &x (f &y &z)
 and f (i &x) &x → e