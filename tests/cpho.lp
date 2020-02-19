constant symbol T : TYPE

symbol f : (T ⇒ T) ⇒ T
symbol u : T
symbol g : T ⇒ T

rule f (λ _, g u) → g u
rule g &X → &X

symbol r : T ⇒ T ⇒ T
rule f (λ x _, &G[x]) → u
rule f (λ x y, r x y) → g u
