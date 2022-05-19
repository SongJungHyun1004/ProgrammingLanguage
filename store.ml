module F = Format

type t = (string * value) list
and value = NumV of int | ClosureV of string * Ast.expr * t

let rec pp_value fmt (v: value) : unit =
        match v with
        | NumV n -> F.fprintf fmt "%d" n
        | ClosureV (x, e, s) -> F.fprintf fmt "<λ%s. %a, %a>" x Ast.pp e pp_store s
and pp_store fmt (s: t) : unit =
        F.fprintf fmt "[%a]"
        (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "; ")
        (fun fmt (x, v) -> F.fprintf fmt "%s -> %a" x pp_value v)) s

let empty = []
let mem (x:string) (s:t) : bool =
        let rec is_mem (x:string) (s:t) : bool =
                match s with
                | [] -> false
                | (var, _)::lst -> if var = x then true else is_mem x lst 
        in
        is_mem x s
let add (x:string) (v: value) (s:t) : t =
        let rec adder (x:string) (v:value) (s:t) (len:int) : t =
                if len = 0 then (x, v) :: s
                else
                        match s with
                        | [] -> []
                        | (var, value)::lst -> if var = x then adder x v lst (len-1)
                                                else adder x v (lst @ [(var, value)]) (len-1)
        in
        adder x v s (List.length s)

let find (x:string) (s:t) : value =
        let rec get (x:string) (s:t) (len:int) : value =
                if len = 0 then failwith ("Free identifier: " ^ x)
                else
                        match s with
                        | [] -> failwith ("Free identifier: " ^ x)
                        | (var, value)::lst -> if var = x then value else get x lst (len-1)
        in
        get x s (List.length s)
