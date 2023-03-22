(*Exception*)
exception UnificationFailure


signature SIGNATURE = sig
    type symbol   (* This type captures the symbols of the signature *)
    val arity   : symbol -> int
    val compare : symbol * symbol -> order  (* So that you can use it during unification *)
end (* module type SIGNATURE *)

structure TypeSig : SIGNATURE = 
struct
    datatype symbol = bool | int | arrow
    
    fun arity arrow = 2
    |   arity (s:symbol) = 0

    fun ordering bool = 0
    |   ordering int = 1
    |   ordering arrow = 2

    fun compare (s1, s2) = Int.compare(ordering s1, ordering s2)
end

signature VAR = sig
    type var
    type ord_key = var
    val fresh : unit -> var
    val user : string -> var
    val toString : var -> string
    val compare : var * var -> order
    val same : var * var -> bool
end


fun zip (x::xs) (y::ys) = ((x,y)::(zip xs ys))

signature UNIFICATION = sig
    type var
    type symbol
    type term
    type telescope
    type equation

    val unify : telescope -> equation -> telescope
    val unifyList : telescope -> equation list -> telescope
end


functor Unify (structure S : SIGNATURE
                structure V : VAR) : UNIFICATION = struct

    type var = V.var
    type symbol = S.symbol

    datatype term = VAR of var
                    | APP of S.symbol * term list

    structure VarMap = RedBlackMapFn(V) 


    type telescope = term VarMap.map 
    type equation = term*term

    fun checkRecursion (VAR x, VAR y) = if (V.same(x, y)) then true else false 
    |   checkRecursion (VAR x, APP(_, ([]))) = false  
    |   checkRecursion (VAR x, APP(s, (t::ts))) = if (checkRecursion (VAR x, t) orelse checkRecursion (VAR x, APP(s, ts))) then true else false
    |   checkRecursion (eq:equation) = false   (*Don't use*)


    fun unify (tel:telescope) (VAR x, t) = if (checkRecursion (VAR x, t)) then raise UnificationFailure else VarMap.singleton(x,t)
    |   unify (tel:telescope) (s, VAR y) = unify tel (VAR y, s)
    |   unify (tel:telescope) (APP (f,fargs), APP (g, gargs)) = unifyList tel (zip fargs gargs)
    
    and unifyList (tel:telescope) [] = tel
    |   unifyList (tel:telescope) ((s,t)::eqns) = unifyList (unify tel (s,t)) eqns 

end



structure typeVar : VAR = struct
    type var = string
    type ord_key = string

    val used = []

    fun user s = s
    fun toString s = s
    fun compare (a,b) = String.compare(a,b)
    fun same (a,b) = (String.compare(a,b) = EQUAL)


    fun diagList [] [] = [#"x"]
    |   diagList [] (x::xs) = if x=(#"x") then [#"y"] else [#"x"]
    |   diagList s [] = s
    |   diagList (x::xs) (y::ys) = if x=y then (x::(diagList xs ys)) else (x::xs)
                            
    fun diag (s1,s2) = String.implode(diagList (String.explode(s1)) (String.explode(s2)))



    (* fun fresh() = let
                    val new = List.foldl diag "" used
                    val used = (new::used)
                in
                    new
                end  *)

    fun fresh () = List.foldl diag "" ["x", "xx", "y"]
end

val f1 = typeVar.fresh()
val f2 = typeVar.fresh()
val f3 = typeVar.fresh()