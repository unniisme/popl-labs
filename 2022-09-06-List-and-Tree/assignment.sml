(*
Q1
map : ('a->'b) -> 'a list -> 'b list
*)

fun map f [] = []
|   map f (x::xs) = ((f x)::(map f xs))


(*
Q2
datatype 'a tree
*)

datatype 'a tree = nullTree | node of 'a tree * 'a * 'a tree


(*
Q3 
treemap : ('a->'b) -> 'a tree -> 'b tree
*)

fun treemap f nullTree = nullTree
|   treemap f (node(t1, x, t2)) = node(treemap f t1, f x, treemap f t2)


(*
Q4
inorder : 'a tree -> 'a list
preorder : 'a tree -> 'a list
postorder : 'a tree -> 'a list
*)

fun inorder nullTree = []
|   inorder (node(t1, x, t2)) = (inorder t1) @ x :: (inorder t2)

fun preorder nullTree = []
|   preorder (node(t1, x, t2)) = x :: (preorder t1) @ (preorder t2)

fun postorder nullTree = []
|   postorder (node(t1, x, t2)) = (postorder t1) @ (postorder t2) @ [x]


(*
Q5
rightRotate : 'a tree -> 'a tree
*)

fun rightRotate (node(node(t0, a, t1), b, t2)) = node(t0, a, node(t1, b, t2))
|   rightRotate t = t