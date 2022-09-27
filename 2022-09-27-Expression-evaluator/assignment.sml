(*Q1.
*)

datatype Expr = Const of real
            |   Var of string
            |   Plus of Expr*Expr
            |   Mul of Expr*Expr

(* Expressions can either be

     1. Constant
     2. Variable
     3. Plus applied on two expressions
     4. Mul applied on two expressions

*)
datatype Stmt = Assignment of Expr*Expr
            |   Print of Expr

(*
   1. Assignment
   2. Print statement
*)


type Program = Stmt list



type Env = real AtomMap.map

(*Q2*)
(*
val addOptions : real option -> real option -> real option
val mulOptions : real option -> real option -> real option

Helper functions
*)
fun addOptions (SOME (x:real)) (SOME (y:real)) = SOME (x+y)
|   addOptions (SOME x) NONE = NONE
|   addOptions NONE (SOME x) = NONE

fun mulOptions (SOME (x:real)) (SOME (y:real)) = SOME (x*y)
|   mulOptions (SOME x) NONE = NONE
|   mulOptions NONE (SOME x) = NONE

(*
val eval = Env -> Expr -> real option
*)
fun eval (env:Env) (Const r) = SOME r
|   eval (env:Env) (Var x) = AtomMap.find(env, Atom.atom(x))
|   eval (env:Env) (Plus (e1, e2)) = addOptions (eval env e1) (eval env e2)
|   eval (env:Env) (Mul (e1, e2)) = mulOptions (eval env e1) (eval env e2)


(*
val execute = Env -> Stmt -> Env
*)
fun execute (env:Env) (Assignment(Var v,e)) = (case (eval env e) of
                                        SOME r => AtomMap.insert(env, Atom.atom(v), r)
                                    |   NONE => env)
|   execute (env:Env) (Assignment(e1,e2)) = env
|   execute (env:Env) (Print e) = let
                                    val _ = case (eval env e) of
                                        SOME r => print (Real.toString(r) ^ "\n")
                                    |   NONE => print ("\n")
                                in
                                    env:Env
                                end

(*
val execute2 = Stmt * Env -> Env
For use in foldl    
*)
fun execute2 (statement, env) = execute env statement
val (newEnv:Env) =  AtomMap.empty

(*
val interpret = Program -> Env
*)
fun interpret (prog:Program) = List.foldl execute2 newEnv prog



(*
Test Case
*)
val prog = [Assignment(Var "x", Const 3.0), Assignment(Var "y", Plus(Const 3.0, Const 2.0)), Print(Mul(Var "x", Var "y"))];
interpret prog;