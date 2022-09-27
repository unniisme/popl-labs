(*
Q1.
*)

(*
foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
*)

fun foldr f s0 [] = s0
|   foldr f s0 (x::xs) = f (x, foldr f s0 xs)

(*
foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
*)

fun foldl f s0 [] = s0
|   foldl f s0 (x::xs) =  foldl f (f(x, s0)) xs


(*
Q2
sumfun : int * int -> int

sum : int list -> int
*)

fun sumfun (a,b) = (a+b)
fun sum l = foldr sumfun 0 l


(*
Q3
*)
(*
partitionfun : ('a -> bool) -> 'a * ('a list * 'a list) -> 'a list * 'a list

partition : ('a -> bool) -> 'a list -> 'a list * 'a list
*)

fun partitionfun f (x, (pos, neg)) = if f x then ((x::pos), neg) else (pos, (x::neg))
fun partition f l = foldr (partitionfun f) ([],[]) l


(*
mapfun : ('a -> 'b) -> 'a * 'b list -> 'b list
returns a summarizing function for the function to be mapped

map : ('a -> 'b) -> 'a list -> 'b list
*)

fun mapfun f (a,b) = ((f a) :: b)
fun map f l = foldr (mapfun f) [] l

(*
reversefun : 'a * 'a list -> 'a list

reverse : 'a list -> 'a list
*)

fun reversefun (x,y) = (x::y)
fun reverse l = foldl reversefun [] l

(*
nthfun : 'a * 'a Find -> 'a Find

nthAux : 'a list * int -> 'a Find

nth : 'a list * int -> 'a option
*)

datatype 'a Find = LookingFor of int
                 | Found      of 'a

fun nthfun (x, LookingFor j) = if (j=0) then Found x else LookingFor (j-1)
|   nthfun (x, Found y) = Found y

fun nthAux (l, i) = foldl nthfun (LookingFor i) l

fun nth (l,i) = case nthAux(l,i) of 
                    LookingFor k => NONE
                |   Found x => SOME x


(*
BONUS
*)

fun rtol f (x, s) = 1