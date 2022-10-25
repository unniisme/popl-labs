(*Q1*)
datatype dbexpr = dbVar of int              (*Variable*)
                | dbApp of dbexpr * dbexpr  (*Application*)
                | dbAbs of dbexpr           (*Abstraction*)



datatype expr = Var of string
                | App of expr*expr  (*Application*)
                | Abs of string*expr (*Abstraction*)


(*
Q2


Assuming there are no free variables
*)

(*
val diagList = fn : char list -> char list -> char list
val diagWrapper = fn : string * string -> string
val newVar = fn : string list -> string

Helper funcitons to generate unique variables
Note that while printing, xx is a single variable while x x is x applied on x
*)
fun diagList [] [] = [#"x"]
|   diagList [] (x::xs) = if x=(#"x") then [#"y"] else [#"x"]
|   diagList s [] = s
|   diagList (x::xs) (y::ys) = if x=y then (x::(diagList xs ys)) else (x::xs)

fun diagWrapper (s1,s2) = String.implode(diagList (String.explode(s2)) (String.explode(s1)))

fun newVar (l:string list) = List.foldl diagWrapper "x" l



(*
val DeBrujin_to_FirstOrder = fn : dbexpr -> string list -> expr

Function converts from DeBrujin Expression to first order expression
*)
fun DeBrujin_to_FirstOrder (dbAbs e)        (vars:string list) = Abs(newVar vars, DeBrujin_to_FirstOrder e ((newVar vars)::vars))
|   DeBrujin_to_FirstOrder (dbApp (x,y))    (vars:string list) = App(DeBrujin_to_FirstOrder x vars, DeBrujin_to_FirstOrder y vars)
|   DeBrujin_to_FirstOrder (dbVar i)        (vars:string list) = Var(List.nth(vars, (i-1)))


(*
Pretty printing functions
val evaldb = fn : dbexpr -> string
val eval = fn : expr -> string
*)
fun evaldb (dbVar i) = Int.toString i
|   evaldb (dbApp (x,y)) = "(" ^ evaldb x ^ ")(" ^ evaldb y ^ ")"
|   evaldb (dbAbs x) = "<L> " ^ evaldb x

fun eval (Var x) = x
|   eval (App(e1, e2)) = (eval e1) ^ " " ^ (eval e2)
|   eval (Abs(v,e2)) = "(<L>" ^ (v) ^ "." ^ (eval e2) ^ ")"


(*Test cases*)
val db1 = dbAbs(dbVar 1)
val db2 = dbAbs(dbAbs(dbVar 2))                     
val db3 = dbAbs(dbApp((dbVar 1), dbAbs(dbVar 2)))

val exp1 = DeBrujin_to_FirstOrder db1 []
val exp2 = DeBrujin_to_FirstOrder db2 []
val exp3 = DeBrujin_to_FirstOrder db3 []

val db1_ev = evaldb db1
val db2_ev = evaldb db2
val db3_ev = evaldb db3

val exp1_ev = eval exp1
val exp2_ev = eval exp2
val exp3_ev = eval exp3