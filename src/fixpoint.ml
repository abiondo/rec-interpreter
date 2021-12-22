(* An infinite lazy sequence of elements *)
type 'a seq =
	| Cons of 'a * (unit -> 'a seq)

(* Applies the Scott-continuous partial function g to the least upper bound of xs *)
let rec lub_apply (xs : 'a seq) (g : 'a -> 'b option) : 'b =
	(* By continuity: g (lub xs) = lub {g x | x in xs}
	 * lub {g x | x in xs} = y iff g x = y for some x in xs. *)
	match xs with
	| Cons (x, tail) ->
		match g x with
		| Some (y) -> y
		| None     -> lub_apply (tail ()) g

(* Stream of powers of a total endofunction, with f^0 = x *)
let rec func_powers (f : 'a -> 'a) (x : 'a) : 'a seq =
	Cons (x, fun () -> func_powers f (f x))

(* Applies the Scott-continuous partial function g to the least fixpoint of the
 * total Scott-continuous endofunction f on the chain-complete partial order
 * with the provided bottom *)
let fix_apply (bottom : 'a) (f : 'a -> 'a) (g : 'a -> 'b option) : 'b =
	(* Theorem 4.37: Kleene-Knaster-Tarski fixpoint iteration,
	 * i.e., fix f = lub {f^n bottom | n in N} *)
	lub_apply (func_powers f bottom) g
