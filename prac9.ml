let interp_fundef (fd: Ast.fundef) (fs: FStore.t) : FStore.t = 
       match fd with
       | FunDef (x, arglst, exp) -> FStore.add x arglst exp fs 
let rec interp_expr (e: Ast.expr) (fs: FStore.t) (s: Store.t) : Value.t =
        let rec toArglst (elst: Ast.expr list) (arglst: Value.t list) : Value.t list =
                match elst with
                | [] -> arglst
                | ek :: lst -> let nk = interp_expr ek fs s in
                               toArglst lst (arglst @ [nk]) 
        in 
        let rec matchStore (prmlst: string list) (arglst: Value.t list) (ss: Store.t) : Store.t =
                match prmlst, arglst with
                | [], [] -> ss
                | prm::plst, arm:: alst -> matchStore plst alst (Store.add prm arm ss)
                | (_, _) -> failwith "Unreachable"
        in
        match e with
        | Num n -> NumV n
        | Id x -> Store.find x s
        | Add (e1, e2) -> 
                        begin
                                let n1 = interp_expr e1 fs s in
                                let n2 = interp_expr e2 fs s in
                                match n1, n2 with
                                | NumV x, NumV y -> NumV (x+y)
                        end
        | Sub (e1, e2) ->
                        begin
                                let n1 = interp_expr e1 fs s in
                                let n2 = interp_expr e2 fs s in
                                match n1, n2 with
                                | NumV x, NumV y -> NumV (x-y)
                        end
        | LetIn (x, e1, e2) -> interp_expr e2 fs (Store.add x (interp_expr e1 fs s) s)
        | Call (x, explst) -> 
                        begin
                                let arglst = toArglst explst [] in
                                let (prmlst, e2) = FStore.find x fs in
                                if List.length arglst <> List.length prmlst
                                then let arg = string_of_int (List.length arglst) in
                                     let prm = string_of_int (List.length prmlst) in
                                     failwith ("Unmatched numbers of arguments: " ^ arg ^ " <> " ^ prm)
                                else let sk = matchStore prmlst arglst [] in
                                interp_expr e2 fs sk
                        end

let interp (p: Ast.prog) : Value.t =
        let rec toFs (fdlst: Ast.fundef list) (fs: FStore.t) : FStore.t = 
                match fdlst with
                | [] -> fs
                | fd :: lst -> toFs lst (interp_fundef fd fs)
        in
        match p with
        | Prog (fdlst, exp) -> interp_expr exp (toFs fdlst []) []
       
