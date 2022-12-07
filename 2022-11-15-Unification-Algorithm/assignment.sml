(*Exception*)
exception UnificationFailure


signature SIGNATURE = sig
    type symbol   (* This type captures the symbols of the signature *)
    val arity   : symbol -> int
    val compare : symbol * symbol -> order  (* So that you can use it during unification *)
    val symbol : string -> symbol (*To make an instance of the symbol*)

    (*Test*)
    val toString : symbol -> string
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

    (*For testing purposes. Error not handled*)
    fun symbol "bool" = bool
    |   symbol "int" = int
    |   symbol "->" = arrow

    fun toString bool = "bool"
    |   toString int = "int"
    |   toString arrow = "->"
end


fun zip (x::xs) (y::ys) = ((x,y)::(zip xs ys))

signature UNIFICATION = sig
    type symbol
    type term
    type telescope
    type equation

    val unify : telescope -> equation -> telescope
    val unifyList : telescope -> equation list -> telescope
    (* val checkRecursion : telescope -> Atom.atom -> bool *)

    (*Testing*)
    val var : Atom.atom -> term   (*Returns a variable*)
    val app : symbol * term list -> term   (*Returns a functional symbol application*)
    val toString : term -> string
end
    

functor Unify (S : SIGNATURE) : UNIFICATION = struct
    type symbol = S.symbol

    datatype term = VAR of Atom.atom
                    | APP of symbol * term list


    type telescope = term AtomMap.map 
    type equation = term*term

    fun checkRecursion (VAR x, VAR y) = if (Atom.same(x, y)) then true else false 
    |   checkRecursion (VAR x, APP(_, ([]))) = false  
    |   checkRecursion (VAR x, APP(s, (t::ts))) = if (checkRecursion (VAR x, t) orelse checkRecursion (VAR x, APP(s, ts))) then true else false
    |   checkRecursion (eq:equation) = false   (*Don't use*)


    fun unify (tel:telescope) (VAR x, t) = if (checkRecursion (VAR x, t)) then raise UnificationFailure else AtomMap.singleton(x,t)
    |   unify (tel:telescope) (s, VAR y) = unify tel (VAR y, s)
    |   unify (tel:telescope) (APP (f,fargs), APP (g, gargs)) = unifyList tel (zip fargs gargs)     (*Could implement checking for arity here*)
    
    and unifyList (tel:telescope) [] = tel
    |   unifyList (tel:telescope) ((s,t)::eqns) = unifyList (unify tel (s,t)) eqns 


    (* Test *)
    fun var a = VAR a
    fun app (s, l) = APP(s,l) 
    fun toString (VAR a) = Atom.toString a
    |   toString (APP (s, [])) = S.toString s
    |   toString (APP (s, x::y::xs)) = (toString x) ^ (S.toString s) ^ (toString y)  (*Works only for TypeSig. Only for testing purposes*) 
end



(* Test cases *)
(*Test case 2*)
structure STLCunify = Unify(TypeSig)

val t1:STLCunify.term = STLCunify.var (Atom.atom "x")
val t2 = STLCunify.app(TypeSig.symbol "->", [STLCunify.var (Atom.atom "y"), STLCunify.var (Atom.atom "z")])
val _ = print("type 1 = " ^ STLCunify.toString t1 ^ "\n")
val _ = print("type 2 = " ^ STLCunify.toString t2 ^ "\n")

val unifier = STLCunify.unify AtomMap.empty (t1,t2)
val _ = print("x <- " ^ STLCunify.toString (AtomMap.lookup(unifier, Atom.atom "x"))  ^ "\n")
(*
Test case results:
t1 = x
t2 = y->z

unify t1, t2
gives x <- (y->z)
*)


(*Test case 2*)
(* val t3 = STLCunify.app(TypeSig.symbol "->", [STLCunify.var (Atom.atom "x"), STLCunify.var (Atom.atom "y")])
val t4 = STLCunify.app(TypeSig.symbol "->", [STLCunify.var (Atom.atom "y"), STLCunify.var (Atom.atom "x")])
val _ = print("type 1 = " ^ STLCunify.toString t3 ^ "\n")
val _ = print("type 2 = " ^ STLCunify.toString t4 ^ "\n")

val unifier = STLCunify.unify AtomMap.empty (t3,t4)
val _ = print("x <- " ^ STLCunify.toString (AtomMap.lookup(unifier, Atom.atom "x"))  ^ "\n") *)

(*
Test case results:
t3 = x->y
t4 = y->x

unify t3, t4 throws an error. Unification is not possible

*)