let rec interp (p: Ast.expr) (s : Store.t) : Store.value = 
        match p with
        | Num n -> NumV n
        | Id x -> Store.find x s
        | Add (e1, e2) -> 
                        begin
                                let n1 = interp e1 s in
                                let n2 = interp e2 s in
                                match n1, n2 with
                                | NumV x, NumV y -> NumV (x+y)
                                | _ -> failwith "Not a number."
                        end
        | Sub (e1, e2) ->
                        begin
                                let n1 = interp e1 s in
                                let n2 = interp e2 s in
                                match n1, n2 with
                                | NumV x, NumV y -> NumV (x-y)
                                | _ -> failwith "Not a number."
                        end
        | LetIn (x, e1, e2) -> interp e2 (Store.add x (interp e1 s) s)
        | App (e1, e2) -> 
                        begin
                                let closure = interp e1 s in
                                let v1 = interp e2 s in
                                match closure with
                                | ClosureV (x, e3, s1) -> interp e3 (Store.add x v1 s1) 
                                | _ -> failwith "Not a function."
                        end
        | Lambda (x, e) -> ClosureV (x, e, s)           


