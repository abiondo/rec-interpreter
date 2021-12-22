(* Binding operator for option *)
let ( >>= ) (o : 'a option) (f : 'a -> 'b) : 'b =
	match o with
	| None     -> None
	| Some (x) -> f x
