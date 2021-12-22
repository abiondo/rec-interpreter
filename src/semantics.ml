module L = Language
module FP = Fixpoint
open Utils

(* Module for the variable environment *)
module Env = struct
	include Map.Make(struct
		type t = L.var
		let compare = compare
	end)

    (* Evaluates a variable *)
	let eval_var (x : L.var) (e : (unit -> 'a) t) : 'a =
		match find_opt x e with
		| None     -> failwith ("Unknown variable " ^ x)
		| Some (v) -> v ()

    (* Merges two environments. If a variable appears in both, e2 shadows e1 *)
    let union_shadow (e1 : 'a t) (e2 : 'a t) : 'a t =
        union (fun k v1 v2 -> Some(v2)) e1 e2
end

(* Type for the variable environment *)
type env = (unit -> L.value option) Env.t

(* Type for a function descriptor *)
type func_desc = {
    sem    : env -> L.value option;
    params : L.var list;
    body   : L.term;
}

(* Module for the function environment *)
module FEnv = struct
	include Map.Make(struct
		type t = L.func
		let compare = compare
	end)

    (* Gets a function *)
    let get_func (name : L.func) (fe : 'a t) : 'a =
		match find_opt name fe with
		| None     -> failwith ("Unknown function " ^ name)
		| Some (v) -> v
end

(* Type for the function environment *)
type fenv = func_desc FEnv.t

(* Semantic function for a term *)
let rec semantic (t : L.term) (fe : fenv) (e : env) : L.value option =
    let binop t1 t2 op =
        semantic t1 fe e >>= fun x1 ->
        semantic t2 fe e >>= fun x2 ->
        Some(op x1 x2)
    in
    let args_env desc args =
        let rec build_args_env names terms =
            match names, terms with
            | [], _ | _, []    -> Env.empty
            | n :: ns, t :: ts -> let tail_e = build_args_env ns ts in
                                  if Env.mem n tail_e
                                  then failwith("Duplicate parameter " ^ n)
                                  else Env.add n (fun () -> semantic t fe e) tail_e
        in
        if List.compare_lengths desc.params args = 0
        then build_args_env desc.params args
        else failwith("Mismatched argument count")
    in
    match t with
    | Num(n)           -> Some(n)
    | Var(x)           -> Env.eval_var x e
    | Add(t1, t2)      -> binop t1 t2 Z.add
    | Sub(t1, t2)      -> binop t1 t2 Z.sub
    | Mul(t1, t2)      -> binop t1 t2 Z.mul
    | Cond(t1, t2, t3) -> semantic t1 fe e >>= fun cond ->
                          if cond = Z.zero then semantic t2 fe e
                          else semantic t3 fe e
    | Call(name, args) -> let desc = FEnv.get_func name fe in
                          desc.sem (Env.union_shadow e (args_env desc args))

(* Functional for function environment fixpoint *)
let fenv_functional (fe : fenv) : fenv =
    FEnv.map (fun d -> { d with sem = semantic d.body fe }) fe

(* Builds the variable environment from definitions *)
let rec build_env (vars : L.var_def list) : env =
	match vars with
	| []      -> Env.empty
	| v :: vs -> let e = build_env vs in
				 if Env.mem v.name e
				 then failwith ("Duplicate variable " ^ v.name)
				 else Env.add v.name (fun () -> Some(v.value)) e

(* Builds the bottom function environment from definitions *)
let rec build_fenv_bottom (funcs : L.func_def list) : fenv =
	match funcs with
	| []      -> FEnv.empty
	| f :: fs -> let fe = build_fenv_bottom fs in
				 if FEnv.mem f.name fe
				 then failwith ("Duplicate function " ^ f.name)
				 else FEnv.add f.name {
                     sem = (fun e -> None);
                     params = f.params;
                     body = f.body;
                 } fe

(* Evaluates a program *)
let eval (prog : L.program) : L.value =
    let e = build_env prog.vars in
    let fe_bottom = build_fenv_bottom prog.funcs in
    FP.fix_apply fe_bottom fenv_functional (fun fe -> semantic prog.term fe e)
