symbol const T : TYPE

symbol f : (T ⇒ T) ⇒ T
symbol u : T
symbol g : T ⇒ T

rule f (λ x, g u) → g u
rule g &X → &X
