datatype expr = Var of Atom.atom
                | App of expr*expr  (*Application*)
                | Abs of Atom.atom*expr (*Abstraction*)


(*
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


(*
val diagList : char list -> char list -> char list
*)
fun diagList [] [] = [#"x"]
|   diagList [] (x::xs) = if x=(#"x") then [#"y"] else [#"x"]
|   diagList s [] = s
|   diagList (x::xs) (y::ys) = if x=y then (x::(diagList xs ys)) else (x::xs)

(*
val diagWrapper : atom * atom -> atom 
*)
fun diagWrapper (a1,a2) = Atom.atom(String.implode(diagList (String.explode(Atom.toString(a2))) (String.explode(Atom.toString(a1)))))

(*
val fresh : AtomSet -> atom
*)
fun fresh s = AtomSet.foldl diagWrapper (Atom.atom("")) s

(*
val eval : expr -> string
*)
fun eval (Var x) = Atom.toString x
|   eval (App(e1, e2)) = (eval e1) ^ " " ^ (eval e2)
|   eval (Abs(v,e2)) = "(<L>" ^ (Atom.toString v) ^ "." ^ (eval e2) ^ ")"

(*---------------*)

datatype Lexpr = LVar of Atom.atom
                | LApp of Lexpr*Lexpr  (*Application*)
                | LAbs of Atom.atom*Lexpr (*Abstraction*)
                | Let of Atom.atom * Lexpr * Lexpr  (*Let*)

datatype LRexpr = LRVar of Atom.atom
                | LRApp of LRexpr*LRexpr  (*Application*)
                | LRAbs of Atom.atom*LRexpr (*Abstraction*)
                | Letrec of Atom.atom * LRexpr * LRexpr    (*Let Rec*)

exception recursionError

(*
val LetToLambda : Lexpr -> expr
*)
fun LetToLambda (Let (x,(*=*) e1,(*in*) e2)) = if AtomSet.member(free (LetToLambda e1), x) then raise recursionError else
                                                App(Abs(x,LetToLambda e2), LetToLambda e1)
|   LetToLambda (LVar x) = Var x 
|   LetToLambda (LApp(x,y)) = App(LetToLambda x,LetToLambda y)
|   LetToLambda (LAbs (x,e)) = Abs(x, LetToLambda e) 


(*Test Cases*)
val Lx = LVar (Atom.atom "x")
val Ly = LVar (Atom.atom "y")
val Lz = LVar (Atom.atom "z")

(*
Let x = y 
    in 
        x z
    end
*)
val f0 = LetToLambda (Let (Atom.atom "x", Ly, LApp(Lx, Lz)))
val f0Ev = eval f0

(*
This test case raises an error.

Let x = x y
    in
        Lambda x.z x
    end

val f1 = LetToLambda(Let (Atom.atom "x", LApp(Lx, Ly), LAbs(Atom.atom "x", LApp(Lz, Lx))))
val f1Ev = eval f1
*)