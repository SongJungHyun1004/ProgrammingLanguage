type state = Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 | Q8 | Q9 | Q10 | Q11 | Q12 | Q13 | Q14 | Q15 | Q16 | Q17 | Q18 | Q19 | Q20

let start_state = Q0

let is_final_state (s:state) : bool =
        match s with
        | Q11 -> true
        | _ ->  false
let transfer_function (s:state) (c:char) : state option =
        match s with
        | Q0 -> begin
                match c with
                | 'a'..'z' -> Some Q1
                | _ -> None
        end
        | Q1 -> begin
                match c with
                | 'a'..'z' | 'A'..'Z' | '_' -> Some Q1
                | '@' -> Some Q2
                | _ -> None
        end
        | Q2 -> begin
                match c with
                | 'c' -> Some Q3
                | 'g' -> Some Q12
                | _ -> None
        end
        | Q3 -> begin
                match c with
                | 'n' -> Some Q4
                | _ -> None
        end
        | Q4 -> begin
                match c with
                | 'u' -> Some Q5
                | _ -> None
        end
        | Q5 -> begin
                match c with
                | '.' -> Some Q6
                | _ -> None
        end
        | Q6 -> begin
                match c with
                | 'a' -> Some Q7
                | _ -> None
        end
        | Q7 -> begin
                match c with
                | 'c' -> Some Q8
                | _ -> None
        end
        | Q8 -> begin
                match c with
                | '.' -> Some Q9
                | _ -> None
        end
        | Q9 -> begin
                match c with
                | 'k' -> Some Q10
                | _ -> None
        end
        | Q10 -> begin
                 match c with
                 | 'r' -> Some Q11
                 | _ -> None
        end
        | Q11 -> begin
                 match c with
                 | _ -> None
        end
        | Q12 -> begin
                 match c with
                 | 'm' -> Some Q13
                 | _ -> None
        end
        | Q13 -> begin
                 match c with
                 | 'a' -> Some Q14
                 | _ -> None
        end
        | Q14 -> begin
                 match c with
                 | 'i' -> Some Q15
                 | _ -> None
        end
        | Q15 -> begin
                 match c with
                 | 'l' -> Some Q16
                 | _ -> None
        end
        | Q16 -> begin
                 match c with
                 | '.' -> Some Q17
                 | _ -> None
        end
        | Q17 -> begin
                 match c with
                 | 'c' -> Some Q18
                 | _ -> None
        end
        | Q18 -> begin
                 match c with
                 | 'o' -> Some Q19
                 | _ -> None
        end
        | Q19 -> begin
                 match c with
                 | 'm' -> Some Q20
                 | _ -> None
        end
        | Q20 -> begin
                 match c with
                 | _ -> None
        end
let cnu_email_lex (str : string) : bool =
        let char_list =  List.of_seq(String.to_seq str) in
        let cur_state = start_state in
        let cur_index = 0 in
        let rec impl (s:state) (i :int) : bool =
                let cur_char = List.nth char_list i in
                let ns_opt = transfer_function s cur_char in
                match ns_opt with
                | None -> false
                | Some s' -> 
                                if (List.length char_list-1) = i then
                                begin
                                        if is_final_state s' then true
                                        else false
                                end
                                else impl s' (i+1)
        in
        impl cur_state cur_index

let email_lex (str : string) : string option =
        let char_list =  List.of_seq(String.to_seq str) in
        let cur_state = start_state in
        let cur_index = 0 in
        let rec impl (s:state) (i :int) : string option =
                let cur_char = List.nth char_list i in
                let ns_opt = transfer_function s cur_char in
                match ns_opt with
                | None -> None
                | Some s' ->
                                if (List.length char_list-1) = i then
                                        begin
                                                match s' with
                                                | Q11 -> Some "cnu"
                                                | Q20 -> Some "gamil"
                                                | _ -> None
                                        end
                                else impl s' (i+1)
        in
        impl cur_state cur_index
