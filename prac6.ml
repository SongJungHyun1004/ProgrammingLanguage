let rec interp (e: Ast.expr) : Value.t = 
        let num (n : Value.t) : int =
                match n with
                | NumV n -> n
        in
        match e with
        | Num n -> NumV n
        | Add (e1, e2) -> NumV(num (interp e1) + num (interp e2))
        | Sub (e1, e2) -> NumV(num (interp e1) - num (interp e2))

