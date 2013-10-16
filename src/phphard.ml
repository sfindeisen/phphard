(*
 * Copyright (C) 2008-2013 Stanislaw Findeisen <stf@eisenbits.com>
 *
 * This file is part of phphard.
 *
 * phphard is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * phphard is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with phphard.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Common;;
open PHPSyntaxTree;;
open PHPSyntaxTreePrinter;;

let makeParseException e lxb =
    let cpos = lxb.Lexing.lex_curr_p
    in
        ParseException(e, cpos.Lexing.pos_fname, cpos.Lexing.pos_lnum, cpos.Lexing.pos_cnum - cpos.Lexing.pos_bol, Lexing.lexeme lxb)
;;

let parse () =
    let lexbuf = Lexing.from_channel stdin
    in
        try
            Phpparser.single_php_source_file Phplexer.token lexbuf
        with
            PHPAnalException(_) as e -> raise (makeParseException e lexbuf)
          | Parsing.Parse_error as e -> raise (makeParseException e lexbuf)
          | Failure(_)          as e -> raise (makeParseException e lexbuf)
          | End_of_file         as e -> raise (makeParseException e lexbuf)
;;

let main () =
    try
        let result = parse ()
        in
            print_endline (toString_PHPSourceFile result);
            print_newline ()
    with
        ParseException(e, fname, lnum, lpos, tok) ->
            print_endline ("Parse error at " ^ fname ^ ":" ^ string_of_int(lnum) ^ ":" ^ string_of_int(lpos) ^ ": " ^ tok);
            print_endline (Printexc.to_string e);
      | e ->
            print_endline ("Toplevel exception: " ^ (Printexc.to_string e));
            raise e
;;

main ();;
