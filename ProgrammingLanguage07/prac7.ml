let rec interp (e: Ast.expr) (s:Store.t) : Value.t =
        match e with
        | Num n -> NumV n
        | Id x -> Store.find x s
        | Add (e1, e2) ->
                        begin
                                let n1 = interp e1 s in
                                let n2 = interp e2 s in
                                match n1,n2 with
                                | NumV x, NumV y -> NumV (x+y)
                        end
        | Sub (e1, e2) ->
                        begin
                                let n1 = interp e1 s in
                                let n2 = interp e2 s in
                                match n1,n2 with
                                | NumV x, NumV y -> NumV (x-y)
                        end
        | LetIn (x, e1, e2) -> interp e2 (Store.add x (interp e1 s) s)
        
