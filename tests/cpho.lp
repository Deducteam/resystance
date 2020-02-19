constant symbol T : TYPE

symbol f : (T ⇒ T) ⇒ T
symbol u : T
symbol g : T ⇒ T

rule f (λ _, g u) → g u
rule g &X → &X
