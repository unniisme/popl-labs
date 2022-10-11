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
Bonus
*)

(*
val BetaReduction expr -> expr -> expr
BetaReduction λx.e M = e [x/M] 
*)
fun BetaReduction (Abs(v, e)) M = subst e v M
|   BetaReduction exp M = exp

(*hmm.... Does simply typing have to be implemented*)

