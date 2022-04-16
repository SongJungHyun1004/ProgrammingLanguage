let rec length (lst : string list) : int =
        match lst with
        | [] -> 0
        | _ :: t -> 1 + (length t)

let rec rev (lst : string list) : string list =
        match lst with
        | [] -> []
        | h :: t -> rev t @ [h]

let is_palin (lst : string list) : bool =
      if lst = rev lst then true else false


let rec sort (lst : int list) : int list =
        let rec insert n (lst : int list) =
                match lst with
                | [] -> [n]
                | h :: t -> if n < h then n :: lst else h :: (insert n t)
        in
        match lst with
        | [] -> []
        | h :: t -> insert h (sort t)
