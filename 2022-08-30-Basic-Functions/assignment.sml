(*
Q1.
curry   : ('a * 'b * 'c -> 'd) -> ('a -> 'b -> 'c -> 'd)
uncurry : ('a -> 'b -> 'c -> d) -> ('a * 'b * 'c -> 'd)
*)
fun curry f a b c = f (a,b,c)
fun uncurry f (a,b,c) = f a b c


(*
Q2.
fst : 'a * 'b -> 'a
snd : 'a * 'b -> 'b
*)
fun fst (a,b) = a
fun snd (a,b) = b


(*
Q3.
length : 'a list -> int
*)
fun length []       = 0 
|   length (x::xs)  = 1+length(xs)


(*
Q4.
reverse : 'a list -> 'a list
reverses a list

Time : O(n)
*)

fun reverseCall []      [] = []
|   reverseCall []      ys = ys
|   reverseCall (x::xs) ys = reverseCall xs (x::ys)

fun reverse xs = reverseCall xs []


(*
Q5.
fib : int -> int
find nth fibonacci number

Time : O(n)
*)

fun fibibibi a b 0 = b
|   fibibibi a b n = fibibibi (a+b) a (n-1)

fun fib n = fibibibi 1 0 n;
