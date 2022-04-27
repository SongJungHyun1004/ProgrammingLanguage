type t = (string * Value.t) list

let empty = []
let mem (x:string) (s:t) : bool =
        let rec is_mem (x:string) (s:t) : bool =
                match s with
                | [] -> false
                | (var, _)::lst -> if var = x then true else is_mem x lst 
        in
        is_mem x s
let add (x:string) (v: Value.t) (s:t) : t =
        let rec adder (x:string) (v:Value.t) (s:t) (len:int) : t =
                if len = 0 then (x, v) :: s
                else
                        match s with
                        | [] -> []
                        | (var, value)::lst -> if var = x then adder x v lst (len-1)
                                                else adder x v (lst @ [(var, value)]) (len-1)
        in
        adder x v s (List.length s)

let find (x:string) (s:t) : Value.t =
        let rec get (x:string) (s:t) (len:int) : Value.t =
                if len = 0 then failwith ("Free identifier: " ^ x)
                else
                        match s with
                        | [] -> failwith ("Free identifier: " ^ x)
                        | (var, value)::lst -> if var = x then value else get x lst (len-1)
        in
        get x s (List.length s)
