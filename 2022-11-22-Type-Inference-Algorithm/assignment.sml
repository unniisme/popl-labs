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
end

(*
structure uniVar : VAR = struct
    (*Fresh variable generation*)
end*)


fun zip (x::xs) (y::ys) = ((x,y)::(zip xs ys))

signature UNIFICATION = sig
    type var
    type symbol
    type term
    type telescope
    type equation

    val unify : telescope -> equation -> telescope
    val unifyList : telescope -> equation list -> telescope
    val checkRecursion : telescope -> var -> bool
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

    fun checkRecursion (tel:telescope) (v:var) = VarMap.inDomain(tel, v)

    fun unify (tel:telescope) (eq:equation) = case eq of
                        (VAR x,t) => if (checkRecursion tel x) then raise Domain else VarMap.singleton(x, t)
                    | (s, VAR y) => if (checkRecursion tel y) then raise Domain else VarMap.singleton(y, s)
                    | (APP (f,fargs), APP (g, gargs)) => unifyList tel (zip fargs gargs)

    and unifyList (tel:telescope) [] = tel
    |   unifyList (tel:telescope) ((s,t)::eqns) = unifyList (unify tel (s,t)) eqns 

end