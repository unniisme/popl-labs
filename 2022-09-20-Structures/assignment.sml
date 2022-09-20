(*
Q1
*)

signature SORT = sig
	type t
 	val sort : t list -> t list
end


signature ORD_KEY =
sig
 	type ord_key

 	val compare : ord_key * ord_key -> order

end


functor QSort ( O : ORD_KEY ) : SORT = 
struct
 	type t = O.ord_key

	fun cmp x y = O.compare(x, y) = GREATER

	fun sort [] = [] 
	|	sort (x::xs) = let 
							val (left, right) = List.partition (cmp x) xs
						in
							(sort left) @ [x] @ (sort right)
						end

end

(*
Q2
*)
structure IntOrd : ORD_KEY = 
struct
	type ord_key = int

	val compare = Int.compare
end

structure intSort = QSort (IntOrd)