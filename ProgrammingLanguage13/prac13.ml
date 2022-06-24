let rec interp (e: Ast.expr) (s : Store.t) : Store.value = 
        match e with
        | Num n -> NumV n
        | Id x -> 
                begin
                        let f = Store.find x s in
                        match f with
                        | FreezedV (e1, s1) -> interp e1 s1
                        | _ -> f
                end
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
                                match closure with
                                | ClosureV (x, e3, s1) -> interp e3 (Store.add x (FreezedV (e2, s)) s1) 
                                | _ -> failwith "Not a function."
                        end
        | Lambda (x, e1) -> ClosureV (x, e1, s)
        | LessThan (e1, e2) -> 
                        begin
                                let n1 = interp e1 s in
                                let n2 = interp e2 s in
                                match n1, n2 with
                                | NumV nn1, NumV nn2 -> if nn1 < nn2 
                                                    then ClosureV ("x", Lambda("y", Id "x"), [])
                                                    else ClosureV ("x", Lambda("y", Id "y"), [])
                                | _ -> failwith "Not a number."
                        end
        
        | RLetIn (x, e1, e2) ->
                        begin
                                match interp e1 s with
                                | ClosureV (x1, e3, s1) ->
                                                let rec s2 = (x, Store.ClosureV(x1, e3, s2)) :: s1 in
                                                interp e2 s2
                                | _ -> failwith "Not a function."
                        end

