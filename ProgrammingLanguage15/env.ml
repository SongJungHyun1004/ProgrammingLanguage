module F = Format

type addr = int
type t = (string * addr) list

let pp fmt (s: t) : unit =
        F.fprintf fmt "[%a]"
        (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "; ")
        (fun fmt (x, a) -> F.fprintf fmt "%s  a%d" x a)) s

let index = ref 0

let new_addr () =
    let nindex = !index in
    let _ = index := nindex + 1 in
    nindex

let empty = []
let mem (x:string) (s:t) : bool =
        let rec is_mem (x:string) (s:t) : bool =
                match s with
                | [] -> false
                | (var, _)::lst -> if var = x then true else is_mem x lst 
        in
        is_mem x s
let add (x:string) (a: addr) (s:t) : t =
        let rec adder (x:string) (a: addr) (s:t) (len:int) : t =
                if len = 0 then (x, a) :: s
                else
                        match s with
                        | [] -> []
                        | (var, address)::lst -> if var = x then adder x a lst (len-1)
                                                else adder x a (lst @ [(var, address)]) (len-1)
        in
        adder x a s (List.length s)

let find (x:string) (s:t) : addr =
        let rec get (x:string) (s:t) (len:int) : addr =
                if len = 0 then failwith ("Free identifier: " ^ x)
                else
                        match s with
                        | [] -> failwith ("Free identifier: " ^ x)
                        | (var, address)::lst -> if var = x then address else get x lst (len-1)
        in
        get x s (List.length s)
