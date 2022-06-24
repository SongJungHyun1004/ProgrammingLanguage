let rec interp_expr (e: Ast.expr) (s : Store.t) : Value.t = 
        match e with
        | Num n -> NumV n
        | Bool b -> BoolV b
        | Name x -> Store.find x s
        | Add (e1, e2) -> 
                begin
                        let n1 = interp_expr e1 s in
                        let n2 = interp_expr e2 s in
                        match n1, n2 with
                        | NumV x, NumV y -> NumV (x + y)
                        | _ -> failwith "Not a number."
                end
        | Sub (e1, e2) ->
                begin
                        let n1 = interp_expr e1 s in
                        let n2 = interp_expr e2 s in
                        match n1, n2 with
                        | NumV x, NumV y -> NumV (x - y)
                        | _ -> failwith "Not a number."
                end
        | Lt (e1, e2) -> 
                begin
                        let n1 = interp_expr e1 s in
                        let n2 = interp_expr e2 s in
                        match n1, n2 with
                        | NumV x, NumV y -> BoolV (x < y)
                        | _ -> failwith "Not a number."
                end
        | Gt (e1, e2) -> 
                begin
                        let n1 = interp_expr e1 s in
                        let n2 = interp_expr e2 s in
                        match n1, n2 with
                        | NumV x, NumV y -> BoolV (x > y)
                        | _ -> failwith "Not a number."
                end
        | Eq (e1, e2) -> 
                begin
                        let v1 = interp_expr e1 s in
                        let v2 = interp_expr e2 s in
                        match v1, v2 with
                        | NumV n1, NumV n2 -> BoolV (n1 = n2)
                        | BoolV b1, BoolV b2 -> BoolV (b1 = b2)
                        | _ -> BoolV false
                end
        | And (e1, e2) -> 
                begin
                        let b1 = interp_expr e1 s in
                        let b2 = interp_expr e2 s in
                        match b1, b2 with
                        | BoolV x, BoolV y -> BoolV (x && y)
                        | _ -> failwith "Not a bool."
                end
        | Or (e1, e2) -> 
                begin
                        let b1 = interp_expr e1 s in
                        let b2 = interp_expr e2 s in
                        match b1, b2 with
                        | BoolV x, BoolV y -> BoolV (x || y)
                        | _ -> failwith "Not a bool."
                end

let rec interp_stmt (e: Ast.stmt) (s: Store.t) : Store.t =
        match e with
        | AssignStmt (x, e) -> Store.add x (interp_expr e s) s
        | IfStmt (e, slist1, slist2) -> 
                begin
                        let b = interp_expr e s in
                        match b with
                        | BoolV true -> toStore slist1 s
                        | BoolV false -> toStore slist2 s
                        | _ -> failwith "Not a bool."
                end
and toStore (slist: Ast.stmt list) (s: Store.t) : Store.t =
                match slist with
                | [] -> s
                | stmt :: list -> toStore list (interp_stmt stmt s)

let interp_prog (e: Ast.prog) : Store.t =
        match e with
        | Program (slist) -> toStore slist Store.empty
                
