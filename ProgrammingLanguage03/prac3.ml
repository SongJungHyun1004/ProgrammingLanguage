type encoded_elem = int * string

let rec decode(lst: encoded_elem list) : string list =
        let rec mk_list(acc: string list) (n: int) (s: string) : string list =
                if n = 0 then acc
                else mk_list (s :: acc) (n-1) s
        in
        match lst with
        | [] -> []
        | (x1, h1) :: t ->  mk_list (decode t) x1 h1


let rec compress (lst : string list) : string list =
        match lst with
        | [] -> []
        | h :: [] -> [h]
        | h :: h2 :: t -> if h = h2 then compress (h :: t) else h :: compress (h2 :: t) 


let pack (lst : string list) : string list list =
        let rec mk_list (lst2 : string list) (lst3 : string list) : string list list =
                match lst2 with
                | [] -> []
                | h :: [] -> [h :: lst3]
                | h :: h2 :: t -> if h = h2 then mk_list (h2 :: t) (h :: lst3) else (h :: lst3) :: mk_list (h2 :: t) []
        in
        mk_list lst []

let encode (lst : string list) : encoded_elem list =
        let rec length (lst : string list) : int =
                match lst with
                | [] -> 0
                | _ :: t -> 1 + (length t)
        in
        let head (lst : string list) : string =
                match lst with
                | [] -> ""
                | h :: _ -> h
        in
        let rec mk_list (lst2 : string list list) : encoded_elem list =
                match lst2 with
                | [] -> []
                | h :: t -> (length h, head h) :: mk_list t
        in
        mk_list (pack lst)


let slice (lst : string list) (s: int) (f: int) : string list =
        let rec length (lst : string list) : int =
                match lst with
                | [] -> 0
                | _ :: t -> 1 + (length t)
        in
        let rec mk_list (lst2 : string list) (count: int) (start: int) (finish: int) : string list=
                match lst2 with
                | [] -> []
                | h :: t -> if count < start then mk_list t (count+1) start finish
                            else if count >= start && count <= finish then h :: (mk_list t (count+1) start finish)
                            else []

        in
        if s > f || s >= length lst || s < 0 || f >= length lst || f < 0 then failwith "error of range"
        else mk_list lst 0 s f

let rotate (lst : string list) (n: int) : string list =
        let rec length (lst : string list) : int = 
                match lst with
                | [] -> 0
                | _ :: t -> 1 + (length t)
        in
        let len = length lst
        in
        let rec convert (input : int) : int =
                if input >= len then (input - len)
                else if input < len && input >= 0 then input
                else convert(input + len)
        in
        let rec mk_list (lst2 : string list) (num: int) : string list =
                if num = 0 then lst2
                else
                        match lst2 with
                        | [] -> []
                        | h :: t -> mk_list(t @ [h]) (num-1)
        in
        mk_list lst (convert n)


