(** Modules in standard ML.

A powerful abstraction feature of standard ML is its module
system. Many languages these days support a rudimentary form of
structures like the module system in languages like Rust and
Haskell. However the module system in ML is much more powerful. What
is particularly interesting is that a ML-like module system can be
incorporated into any language (not necessarily a functional
programming language) and can add to the power of the language. What
more, you can completely eliminate the module system at compile time
and incur zero cost; infact the first step in the mlton compiler is
the defunctorisation step that removes all such modules details.

There are three things that constitute the module system of ML:

* Structures: There are things that packages a collection of types,
  values, and functions into a new name space.

* Signatures: These describe the interface that a given structure
  defines.

* Functors: This are "functions on structures", i.e. they take as input
  structures and give out other structures.

These three component play a role analogues to values, types and
functions respectively in the world of SML expressions.

 *)


(** ** Structure

Save the code below into a file and load it into your sml interpreter.

 *)

structure A
= struct

type t  = int
val  v  = 42
fun f x = x + 1

end

(** ** Accessing members.

A member of a structure can be accessed using the dot notation: for
example after loading the above structure into your interpreter, you
can access the members t, v, and f by using A.t, A.v, and A.f
respectively.

Exercise: Try it out.

You can "open" up a structure using the open command as given below;
however may people consider this bad idea as in large code base, there
might multiple modules that export the same name and the module name
acts as a documentation on which function we meant.

Exercise: Try out opening the module A.

 *)
open A

(** ** Some standard modules.

You might have already used a lot of list functions. Many additional
functions are available in the List structure exposed by the standard
library. For documentation see
<http://sml-family.org/Basis/list.html#LIST:SIG:SPEC>

 *)
open List

(** ** Signatures.

Often one would want to hide private functions that is only accessible
to code inside the module. By fixing the signatures of a structure we
can control what names a module exposes to the outside world. In the
definition of the structure B below, we have hidden fromBool and
fromInt from the outside world by setting a signature for it.

Exercise: Load the definition below and see what you get when you try
B.fromBool (also try opening B). Retry the same definition without the
signature part.

 *)

structure B :
	  sig
	      type t
	      val  v : t
	      val  f : t -> t
	  end
= struct

  datatype t = Foo of int | Bar of bool

  val v = Foo 42

  fun fromInt x = x > 0
  fun fromBool true  = 1
    | fromBool false = 0


  fun f (Foo x) = Bar (fromInt x)
    | f (Bar x) = Foo (fromBool x)


end

(** Signatures can be given a name as well.

*)


signature MYSIG = sig
    type t
    val  v : t
    val  f : t -> t
end

structure C : MYSIG = struct

type t   = int
val  v   = 42
fun  f x = x + 1
end

(** ** Opaque signatures

Signature declaration can enforce a stronger form of hiding using the
opaque signature declaration. Consider the following definition.


 *)
structure AO :> MYSIG = struct

type t = int
val  v = 100
val  f = fn x => x + 1
val  bar = 43
end

(**

The above definition of the structure AO not only hides bar from the
outside world (because MYSIG does not have a bar member), it also
hides the fact that the type AO.t is int. Outside the module one has
no way to manipulate elements of type AO.t.

Exercise: Try out the following definitions by uncommenting the below
lines.

*)
(*

val x : C.t = 43
val y : AO.t = 43

*)

(**

Such opaque signatures allows one to define truly abstract types which
can only be manipulated by the members of the structure.

*)

(** ** Functors

The final important piece in the module system is the functor. A
functor should be seen as a function that takes structures as input
and returns another structure. Given below is a functor that takes any
structure that satisfies the MYSIG sig and produces another one with
the same signature.

*)

functor MyFunctor ( X : MYSIG) : MYSIG = struct

type t = X.t
val  v = X.v

fun  f x = X.f (X.f x) (* apply the function in X twice *)

end

(** You can create new structures thus *)


structure ANew = MyFunctor (A)
structure BNew = MyFunctor (B)

(**

Functors can be used to define abstract algorithm which only makes use
of the interface provided by the input structure's signature. Notice
that MyFunctor cannot assume anything about X other than the fact that
it is a structure of the signature MYSIG.

*)
