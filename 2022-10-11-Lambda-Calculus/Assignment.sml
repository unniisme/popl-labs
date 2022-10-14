(*Q1*)
datatype expr = Var of Atom.atom
                | App of expr*expr  (*Application*)
                | Abs of Atom.atom*expr (*Abstraction*)


(*
Q2
val free : expr -> atom set
val bound : expr -> atom set
*)
fun free (Var a) = AtomSet.singleton a
|   free (App (e1,e2)) = AtomSet.union ((free e1),(free e2))
|   free (Abs (v, e)) = AtomSet.subtract((free e), v)

fun bound (Var a) = AtomSet.empty
|   bound (App (e1,e2)) = AtomSet.union ((bound e1),(bound e2))
|   bound (Abs (v,e)) = AtomSet.singleton v



(*
Q3
val subst : expr -> Atom.atom -> expr -> expr
subst e1 x e2 = e1 [x/e2]
*)
fun subst (Var y) x e = if Atom.same(x,y) then e else (Var y)
|   subst (App (e1,e2)) x e = let
                                val es1 = subst e1 x e
                                val es2 = subst e2 x e
                            in
                                App (es1, es2)
                            end
|   subst (Abs (v, e0)) x e = if Atom.same(v, x) then  (Abs (v, e0)) else Abs(v, subst e0 x e)



(*Q4*)
(*
val diagList : char list -> char list -> char list
*)
fun diagList [] [] = [#"x"]
|   diagList [] (x::xs) = if x=(#"x") then [#"y"] else [#"x"]
|   diagList s [] = s
|   diagList (x::xs) (y::ys) = if x=y then (x::(diagList xs ys)) else (x::xs)

(*
val diag : string -> string -> string
*)
fun diag s1 s2 = String.implode(diagList (String.explode(s1)) (String.explode(s2)))

(*
val diagA : string -> atom -> string 
*)
fun diagA s1 a = diag s1 (Atom.toString a)

(*
val diagWrapper : atom * atom -> atom 
*)
fun diagWrapper (a1,a2) = Atom.atom(diagA (Atom.toString(a2)) a1)

(*
val fresh : AtomSet -> atom
*)
fun fresh s = AtomSet.foldl diagWrapper (Atom.atom("")) s





(*
--------------------------------------
Bonus
--------------------------------------
*)

(*
BetaReduction λx.e M = e [x/M] 

val BetaReduction : expr -> expr
*)
fun BetaReduction (App((Abs(v, e)), M)) = subst e v M
|   BetaReduction exp = exp


(*
val NormalOrder : expr -> expr
*)
fun NormalOrder (Var a) = Var a
|   NormalOrder (App(Abs(v,e),e2)) = BetaReduction(App((Abs(v, NormalOrder(e))), NormalOrder(e2)))
|   NormalOrder (App(e1,e2)) = App(NormalOrder(e1),NormalOrder(e2))
|   NormalOrder (Abs(v,e)) = Abs(v, NormalOrder(e))


(*
val eval : expr -> string
Wrapper function to get string representation of expression
<L> is lambda
*)
fun eval (Var x) = Atom.toString x
|   eval (App(e1, e2)) = (eval e1) ^ " " ^ (eval e2)
|   eval (Abs(v,e2)) = "<L>" ^ (Atom.toString v) ^ "." ^ (eval e2)




(*----Test cases----*)
val x = (Atom.atom "x")
val y = (Atom.atom "y") 
val z = (Atom.atom "z") 

(*
1
exp1 = (λx.(xx))(y x)
exp1 -beta-> exp2
exp2 is expected to be y x y x
*)
val exp1 = App(Abs(x, App(Var x, Var x)), App(Var y, Var x));
val exp2 = BetaReduction exp1
val exp2_string = eval exp2

(*
2
exp3 = (λx.(x y))(λz.x y)
exp3 -beta-> exp4  (on the leftmost λ)
exp4 is expected to be λz.x y y
*)
val exp3 = App(Abs(x, App(Var x, Var y)), App(Abs(z, Var x), Var y));
val exp4 = BetaReduction exp3
val exp4_string = eval exp4


(*
3
exp3 = (λx.(x y))(λz.x y)
exp3 -beta*-> exp5  (Normalised)
exp5 is expected to be x y
*)
val exp3 = App(Abs(x, App(Var x, Var y)), App(Abs(z, Var x), Var y));
val exp5 = NormalOrder exp3
val exp5_string = eval exp5


(*
4
exp6 = (λx.(x y))(λz.(x z))
exp6 -beta*-> exp7  (Normalised)
exp7 is expected to be x y
*)
val exp6 = App(Abs(x, App(Var x, Var y)), Abs(z, App(Var x,Var z)));
val exp7 = NormalOrder exp6
val exp7_string = eval exp7

(*
Seems like this particular normalization stratagy doesn't work for this case.
*)

(*
I only managed to implement this one sorry
*)


