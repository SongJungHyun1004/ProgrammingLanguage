let rec interp (e: Ast.expr) (s : Store.t) : Store.value = 
        match e with
        | Num n -> NumV n
        | Bool b -> BoolV b
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
        | Lambda (x, e1) -> ClosureV (x, e1, s)
        | LessThan (e1, e2) -> 
                        begin
                                let n1 = interp e1 s in
                                let n2 = interp e2 s in
                                match n1, n2 with
                                | NumV x, NumV y -> if x < y then BoolV true else BoolV false
                                | _ -> failwith "Not a number."
                        end
        | Cond (e1, e2, e3) -> 
                        begin
                                let b = interp e1 s in
                                match b with
                                | BoolV true -> interp e2 s
                                | BoolV false -> interp e3 s
                                | _ -> failwith "Not a bool."
                        end

let rec interp_c (e: AstC.expr) (s : StoreC.t) : StoreC.value = 
        match e with
        | Num n -> NumV n
        | Id x -> StoreC.find x s
        | Add (e1, e2) -> 
                        begin
                                let n1 = interp_c e1 s in
                                let n2 = interp_c e2 s in
                                match n1, n2 with
                                | NumV x, NumV y -> NumV (x+y)
                                | _ -> failwith "Not a number."
                        end
        | Sub (e1, e2) ->
                        begin
                                let n1 = interp_c e1 s in
                                let n2 = interp_c e2 s in
                                match n1, n2 with
                                | NumV x, NumV y -> NumV (x-y)
                                | _ -> failwith "Not a number."
                        end
        | LetIn (x, e1, e2) -> interp_c e2 (StoreC.add x (interp_c e1 s) s)
        | App (e1, e2) -> 
                        begin
                                let closure = interp_c e1 s in
                                let v1 = interp_c e2 s in
                                match closure with
                                | ClosureV (x, e3, s1) -> interp_c e3 (StoreC.add x v1 s1) 
                                | _ -> failwith "Not a function."
                        end
        | Lambda (x, e1) -> ClosureV (x, e1, s)
        | LessThan (e1, e2) -> 
                        begin
                                let n1 = interp_c e1 s in
                                let n2 = interp_c e2 s in
                                match n1, n2 with
                                | NumV x, NumV y -> if x < y then NumV 1 else NumV 0
                                | _ -> failwith "Not a number."
                        end
        | Cond (e1, e2, e3) -> 
                        begin
                                let n = interp_c e1 s in
                                match n with
                                | NumV 0 -> interp_c e3 s
                                | NumV _ -> interp_c e2 s
                                | _ -> failwith "Not a number."
                        end

let rec interp_fp (e: AstFP.expr) (s : StoreFP.t) : StoreFP.value = 
        match e with
        | Num n -> NumV n
        | Id x -> StoreFP.find x s
        | Add (e1, e2) -> 
                        begin
                                let n1 = interp_fp e1 s in
                                let n2 = interp_fp e2 s in
                                match n1, n2 with
                                | NumV x, NumV y -> NumV (x+y)
                                | _ -> failwith "Not a number."
                        end
        | Sub (e1, e2) ->
                        begin
                                let n1 = interp_fp e1 s in
                                let n2 = interp_fp e2 s in
                                match n1, n2 with
                                | NumV x, NumV y -> NumV (x-y)
                                | _ -> failwith "Not a number."
                        end
        | LetIn (x, e1, e2) -> interp_fp e2 (StoreFP.add x (interp_fp e1 s) s)
        | App (e1, e2) -> 
                        begin
                                let closure = interp_fp e1 s in
                                let v1 = interp_fp e2 s in
                                match closure with
                                | ClosureV (x, e3, s1) -> interp_fp e3 (StoreFP.add x v1 s1) 
                                | _ -> failwith "Not a function."
                        end
        | Lambda (x, e1) -> ClosureV (x, e1, s)
        | LessThan (e1, e2) -> 
                        begin
                                let n1 = interp_fp e1 s in
                                let n2 = interp_fp e2 s in
                                match n1, n2 with
                                | NumV nn1, NumV nn2 -> if nn1 < nn2 
                                                    then ClosureV ("x", Lambda("y", Id "x"), [])
                                                    else ClosureV ("x", Lambda("y", Id "y"), [])
                                | _ -> failwith "Not a number."
                        end
