module L = Language
module S = Semantics
open Utils

(* Parses a program *)
let parse (c : in_channel) : L.program =
	let lexbuf = Lexing.from_channel c in
	try
		Parser.program Lexer.token lexbuf
	with exn ->
		begin
			let curr = lexbuf.Lexing.lex_curr_p in
			let line = curr.Lexing.pos_lnum in
			let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
			let tok = Lexing.lexeme lexbuf in
			Printf.eprintf
				"Parsing error: %s: line %d, column %d, token \"%s\"\n"
				(Printexc.to_string exn) line cnum tok;
			exit 1
		end

let program = parse stdin
let result = S.eval program

let () = Printf.printf "%s\n" (Z.to_string result);
