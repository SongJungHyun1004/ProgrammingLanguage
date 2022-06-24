type t = (string * (string list * Ast.expr)) list
let empty : t = []
let mem (x: string) (fs: t) : bool =
        let rec is_mem (x: string) (fs: t) : bool =
                match fs with
                | [] -> false
                | (fname, _) :: lst -> if fname = x then true else is_mem x lst
        in
        is_mem x fs
let add (x: string) (pl: string list) (e: Ast.expr) (fs: t) : t =
        let rec adder (x: string) (pl: string list) (e: Ast.expr) (fs: t) (len: int) : t =
                if len = 0 then (x, (pl, e)) :: fs
                else
                        match fs with
                        | [] -> []
                        | (fname, (arglst, exp)) :: lst -> if fname = x then adder x pl e lst (len-1)
                                                           else adder x pl e (lst @ [(fname, (arglst, exp))]) (len-1)
        in
        adder x pl e fs (List.length fs)
let find (x: string) (fs: t) : (string list * Ast.expr) =
        let rec get (x: string) (fs: t) (len: int) : (string list * Ast.expr) =
                if len = 0 then failwith ("Undefined function: " ^ x)
                else
                        match fs with
                        | [] -> failwith ("Undefined function: " ^ x)
                        | (fname, (arglst, exp)) :: lst -> if fname = x then (arglst, exp) else get x lst (len-1)
        in
        get x fs (List.length fs)
