module F = Format

type expr = Num of int
  | Name of string
  | Bool of bool
  | Add of expr * expr
  | Sub of expr * expr
  | Lt of expr * expr
  | Gt of expr * expr
  | Eq of expr * expr
  | And of expr * expr
  | Or of expr * expr

type stmt = AssignStmt of string * expr
  | IfStmt of expr * stmt list * stmt list

type prog = Program of stmt list

let rec pp_expr fmt e = 
  match e with
  | Num n -> F.fprintf fmt "%d" n
  | Name x -> F.fprintf fmt "%s" x
  | Bool b -> F.fprintf fmt "%b" b
  | Add (e1, e2) -> F.fprintf fmt "%a + %a" pp_expr e1 pp_expr e2
  | Sub (e1, e2) -> F.fprintf fmt "%a - %a" pp_expr e1 pp_expr e2
  | Lt (e1, e2) -> F.fprintf fmt "%a < %a" pp_expr e1 pp_expr e2
  | Gt (e1, e2) -> F.fprintf fmt "%a > %a" pp_expr e1 pp_expr e2
  | Eq (e1, e2) -> F.fprintf fmt "%a == %a" pp_expr e1 pp_expr e2
  | And (e1, e2) -> F.fprintf fmt "%a && %a" pp_expr e1 pp_expr e2
  | Or (e1, e2) -> F.fprintf fmt "%a || %a" pp_expr e1 pp_expr e2

let rec pp_stmt fmt s = 
  match s with
  | AssignStmt (x, e) -> F.fprintf fmt "%s = %a;" x pp_expr e
  | IfStmt (e, s1, s2) -> F.fprintf fmt "if %a { %a } else { %a }" pp_expr e
      (F.pp_print_list (fun fmt s -> F.fprintf fmt "%a" pp_stmt s)) s1
      (F.pp_print_list (fun fmt s -> F.fprintf fmt "%a" pp_stmt s)) s2

let pp fmt (Program sl) =
  F.fprintf fmt "%a" (F.pp_print_list 
    ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
    (fun fmt s -> F.fprintf fmt "%a" pp_stmt s)) sl

let rec pp_expr_ast fmt e = 
  match e with
  | Num n -> F.fprintf fmt "(Num %d)" n
  | Name x -> F.fprintf fmt "(Name %s)" x
  | Bool b -> F.fprintf fmt "(Bool %b)" b
  | Add (e1, e2) -> F.fprintf fmt "(Add %a, %a)" pp_expr_ast e1 pp_expr_ast e2
  | Sub (e1, e2) -> F.fprintf fmt "(Sub %a, %a)" pp_expr_ast e1 pp_expr_ast e2
  | Lt (e1, e2) -> F.fprintf fmt "(Lt %a, %a)" pp_expr_ast e1 pp_expr_ast e2
  | Gt (e1, e2) -> F.fprintf fmt "(Gt %a, %a)" pp_expr_ast e1 pp_expr_ast e2
  | Eq (e1, e2) -> F.fprintf fmt "(Eq %a, %a)" pp_expr_ast e1 pp_expr_ast e2
  | And (e1, e2) -> F.fprintf fmt "(And %a, %a)" pp_expr_ast e1 pp_expr_ast e2
  | Or (e1, e2) -> F.fprintf fmt "(Or %a, %a)" pp_expr_ast e1 pp_expr_ast e2

let rec pp_stmt_ast fmt s = 
  match s with
  | AssignStmt (x, e) -> F.fprintf fmt "(AssignStmt %s, %a)" x pp_expr_ast e
  | IfStmt (e, s1, s2) -> F.fprintf fmt "(IfStmt %a, [%a], [%a])" pp_expr_ast e
      (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "; ") (fun fmt s ->
        F.fprintf fmt "%a" pp_stmt_ast s)) s1
      (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "; ") (fun fmt s ->
        F.fprintf fmt "%a" pp_stmt_ast s)) s2

let pp_ast fmt (Program sl) =
  F.fprintf fmt "(Program [%a])" (F.pp_print_list 
    ~pp_sep:(fun fmt () -> F.fprintf fmt "; ") 
    (fun fmt s -> F.fprintf fmt "%a" pp_stmt_ast s)) sl

