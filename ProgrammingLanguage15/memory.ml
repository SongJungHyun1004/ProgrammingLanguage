module F = Format

type t = (Env.addr * Value.t) list

let pp fmt (s: t) : unit =
        F.fprintf fmt "[%a]"
        (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "; ")
        (fun fmt (a, v) -> F.fprintf fmt "a%d  %a" a Value.pp v)) s

let empty = []
let mem (a:Env.addr) (s:t) : bool =
        let rec is_mem (a:Env.addr) (s:t) : bool =
                match s with
                | [] -> false
                | (address, _)::lst -> if address = a then true else is_mem a lst 
        in
        is_mem a s
let add (a:Env.addr) (v: Value.t) (s:t) : t =
        let rec adder (a:Env.addr) (v:Value.t) (s:t) (len:int) : t =
                if len = 0 then (a, v) :: s
                else
                        match s with
                        | [] -> []
                        | (address, value)::lst -> if address = a then adder a v lst (len-1)
                                                else adder a v (lst @ [(address, value)]) (len-1)
        in
        adder a v s (List.length s)

let find (a:Env.addr) (s:t) : Value.t =
        let rec get (a:Env.addr) (s:t) (len:int) : Value.t =
                if len = 0 then failwith "Uninitialized address."
                else
                        match s with
                        | [] -> failwith "Uninitialized address."
                        | (address, value)::lst -> if address = a then value else get a lst (len-1)
        in
        get a s (List.length s)
