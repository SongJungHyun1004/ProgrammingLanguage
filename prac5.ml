module ListSet = struct
        type t = int list
        
        let empty : t = []
        let add (elem : int) (set : t) : t =
                let rec add2(elem : int) (set : t) (len : int) : t =
                        if len = 0 then elem :: set
                        else 
                                match set with
                                | [] -> empty
                                | h :: tt -> if elem = h then add2 elem tt (len-1) 
                                             else add2 elem (tt @ [h]) (len-1)
                in
                add2 elem set (List.length set)
        let union (set1 : t) (set2 : t) : t =
                let rec rmv_common (lst1 : t)(lst2 : t)(len : int) : t =
                        if len = 0 then lst2 
                        else
                                match lst2 with
                                | [] -> empty
                                | h :: tt -> if List.mem h lst1 
                                               then rmv_common lst1 tt (len-1)
                                               else rmv_common lst1 (tt @ [h]) (len-1)
                in
                set1 @ rmv_common set1 set2 (List.length set2)
        let intersection (set1 : t) (set2 : t) : t =
                let rec common (lst1 : t)(lst2 : t)(len : int) : t =
                        if len = 0 then lst1
                        else
                                match lst1 with
                                | [] -> empty
                                | h :: tt -> if List.mem h lst2
                                               then common (tt @ [h]) lst2 (len-1)
                                               else common tt lst2 (len-1)
                in
                common set1 set2 (List.length set1)
        let relative_complement (set1 : t) (set2 : t) : t =
                let rec rmv_common (lst1 : t)(lst2 : t)(len : int) : t =
                        if len = 0 then lst1
                        else
                                match lst1 with
                                | [] -> empty
                                | h :: tt -> if List.mem h lst2
                                               then rmv_common tt lst2 (len-1)
                                               else rmv_common (tt @ [h]) lst2 (len-1)
                in
                rmv_common set1 set2 (List.length set1)
end

module ListMap = struct
        type t = (string * int) list

        let empty : t = []
        let add (key : string) (value : int) (map : t) : t =
                let rec find (key : string) (value : int) (map : t) (len : int) : t =
                        if len = 0 then (key, value) :: map
                        else
                                match map with
                                | [] -> empty
                                | (k, v) :: tt -> if key = k then find key value tt (len-1)
                                                  else find key value (tt @ [(k, v)]) (len-1)
                in
                find key value map (List.length map)
        let get (key : string) (map : t) : int =
                let rec find (key : string) (map : t) (len : int) : int =
                        if len = 0 then failwith "No such key here."
                        else
                                match map with
                                | [] -> failwith "No such key here."
                                | (k, v) :: tt -> if key = k then v else find key tt (len-1)
                in
                find key map (List.length map)
        let remove (key : string) (map : t) : t =
                let rec find (key : string) (map : t) (len : int) (result : t) : t =
                        if len = 0 then failwith "No such key here."
                        else
                                match map with
                                | [] -> failwith "No such key here."
                                | (k, v) :: tt -> if key = k then result @ tt 
                                                  else find key tt (len-1) (result @ [(k, v)])
                in
                find key map (List.length map) []
        let values (map : t) : ListSet.t =
                let rec valueSet (map : t) (result : ListSet.t) : ListSet.t =
                        match map with
                        | [] -> result
                        | (_, v) :: tt -> if List.mem v result then valueSet tt result
                                          else valueSet tt (result @ [v])
                in
                valueSet map []
end

type value = Int of int | String of string
type node = N of value * node list

let preorder (n: node) : value list = 
        let rec pre2 (n: node) : value list =
                let rec pre (lst: node list)(len : int) : value list =
                        match lst with
                        | [] -> []
                        | n :: t -> if len = 0 then []
                                    else (pre2 n) @ (pre t (len-1))
                in
                match n with
                | N (h, []) -> [h]
                | N (v, t) -> v :: (pre t (List.length t))
        in
        pre2 n

 
let postorder (n: node) : value list =
        let rec post2 (n: node) : value list =
                let rec post (lst: node list)(len : int) : value list =
                        match lst with
                        | [] -> []
                        | n :: t -> if len = 0 then []
                                    else (post2 n) @ (post t (len-1))
                in
                match n with
                | N (h, []) -> [h]
                | N (v, t) -> (post t (List.length t)) @ [v]
        in
        post2 n

let levelorder (n: node) : value list =
        let rec level (queue: node list) : value list =
                match queue with
                | [] -> []
                | N(v, t) :: q -> v :: (level (q @ t))
        in
        match n with
        | N (h, []) -> [h]
        | N (v, t) -> level [N (v, t)]
