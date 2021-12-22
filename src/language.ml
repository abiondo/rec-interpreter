(* The name of a variable *)
type var = string
(* The name of a function *)
type func = string
(* The value of a variable *)
type value = Z.t

(* A term *)
type term =
	| Num  of value
	| Var  of var
	| Add  of term * term
	| Sub  of term * term
	| Mul  of term * term
	| Cond of term * term * term
	| Call of func * term list

(* A variable definition *)
type var_def = {
	name  : var;
	value : value;
}

(* A function definition *)
type func_def = {
	name   : func;
	params : var list;
	body   : term;
}

(* A program *)
type program = {
	vars  : var_def list;
	funcs : func_def list;
	term  : term;
}
