module F = Format

type t =
    | NumV of int
    | BoolV of bool
    | AddrV of Env.addr

let pp fmt (v: t) : unit =
    match v with
    | NumV n -> F.fprintf fmt "%d" n
    | BoolV b -> F.fprintf fmt "%b" b
    | AddrV a -> F.fprintf fmt "a%d" a
