let rec interp_expr (e: Ast.expr) (env: Env.t) (mem : Memory.t) : Value.t = 
        match e with
        | Num n -> NumV n
        | Bool b -> BoolV b
        | Name x -> let a = Env.find x env in Memory.find a mem
        | Ref x -> AddrV (Env.find x env)
        | Add (e1, e2) -> 
                begin
                        let n1 = interp_expr e1 env mem in
                        let n2 = interp_expr e2 env mem in
                        match n1, n2 with
                        | NumV x, NumV y -> NumV (x + y)
                        | _ -> failwith "Not a number."
                end
        | Sub (e1, e2) ->
                begin
                        let n1 = interp_expr e1 env mem in
                        let n2 = interp_expr e2 env mem in
                        match n1, n2 with
                        | NumV x, NumV y -> NumV (x - y)
                        | _ -> failwith "Not a number."
                end
        | Lt (e1, e2) -> 
                begin
                        let n1 = interp_expr e1 env mem in
                        let n2 = interp_expr e2 env mem in
                        match n1, n2 with
                        | NumV x, NumV y -> BoolV (x < y)
                        | _ -> failwith "Not a number."
                end
        | Gt (e1, e2) -> 
                begin
                        let n1 = interp_expr e1 env mem in
                        let n2 = interp_expr e2 env mem in
                        match n1, n2 with
                        | NumV x, NumV y -> BoolV (x > y)
                        | _ -> failwith "Not a number."
                end
        | Eq (e1, e2) -> 
                begin
                        let v1 = interp_expr e1 env mem in
                        let v2 = interp_expr e2 env mem in
                        match v1, v2 with
                        | NumV n1, NumV n2 -> BoolV (n1 = n2)
                        | BoolV b1, BoolV b2 -> BoolV (b1 = b2)
                        | AddrV a1, AddrV a2 -> BoolV (a1 = a2)
                        | _ -> BoolV false
                end
        | And (e1, e2) -> 
                begin
                        let b1 = interp_expr e1 env mem in
                        let b2 = interp_expr e2 env mem in
                        match b1, b2 with
                        | BoolV x, BoolV y -> BoolV (x && y)
                        | _ -> failwith "Not a bool."
                end
        | Or (e1, e2) -> 
                begin
                        let b1 = interp_expr e1 env mem in
                        let b2 = interp_expr e2 env mem in
                        match b1, b2 with
                        | BoolV x, BoolV y -> BoolV (x || y)
                        | _ -> failwith "Not a bool."
                end

let rec interp_stmt (e: Ast.stmt) (env: Env.t) (mem : Memory.t) : Env.t * Memory.t =
        match e with
        | VarDeclStmt x -> Env.add x (Env.new_addr ()) env, mem
        | LoadStmt (x, e) -> 
                begin
                        let a = interp_expr e env mem in
                        match a with
                        | AddrV address -> env, Memory.add (Env.find x env) (Memory.find address mem) mem
                        | _ -> failwith "Not an address."
                end
        | StoreStmt (e1, e2) -> 
                begin
                        let a = interp_expr e1 env mem in
                        match a with
                        | AddrV address -> 
                                let v = interp_expr e2 env mem in
                                env, Memory.add address v mem
                        | _ -> failwith "Not an address."
                end
        | IfStmt (e, slist1, slist2) -> 
                begin
                        let b = interp_expr e env mem in
                        match b with
                        | BoolV true -> let _, m = toStore slist1 env mem in env, m
                        | BoolV false -> let _, m = toStore slist2 env mem in env, m
                        | _ -> failwith "Not a bool."
                end
        | WhileStmt (e, slist) -> 
                begin
                        let b = interp_expr e env mem in
                        match b with
                        | BoolV false -> env, mem
                        | BoolV true -> let _, m = toStore slist env mem in interp_stmt (WhileStmt(e, slist)) env m
                        | _ -> failwith "Not a bool."
                end
and toStore (slist: Ast.stmt list) (env: Env.t) (mem : Memory.t) : Env.t * Memory.t =
                match slist with
                | [] -> env, mem
                | stmt :: list -> let env1, m = interp_stmt stmt env mem in toStore list env1 m

let interp_prog (e: Ast.prog) : Env.t * Memory.t =
        match e with
        | Program slist -> toStore slist Env.empty Memory.empty
                