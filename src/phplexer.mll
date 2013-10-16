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

{
open Phpparser;;
open Lexing;;

(* called on newline token *)
let newlineUpdate lxb =
    let cpos = lxb.lex_curr_p
    in
        lxb.lex_curr_p <- { cpos with
            Lexing.pos_lnum = cpos.Lexing.pos_lnum + 1;
            Lexing.pos_bol  = cpos.Lexing.pos_cnum;
        }
;;
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let identifier = (alpha|'_')((alpha|digit|'_')* )

(* ordinary string character (without: single/double quotes, backslash) *)
let string_char = alpha|digit|['-' '!' '@' '#' '$' '%' '^' '&' '*' '(' ')' '_' '=' '+' '[' ']' '{' '}' '|' ';' ':' '<' '>' ',' '.' '/' '?' '~' '`' ' ']

rule token = parse
    "<?php"        { T_PHPSL }
  | "?>"           { T_PHPE }

(* logical operators *)
  | "&&"           { T_AMPERSAND_2 }
  | "and"          { T_AND }
  | "||"           { T_BAR_2 }
  | "or"           { T_OR }
  | "xor"          { T_XOR }
  | '!'            { T_EXCLAMATION }

(* bitwise operators *)
  | "&"            { T_AMPERSAND }
  | "|"            { T_BAR }
  | "^"            { T_CARET }
  | '~'            { T_TILDE }

(* predefined types *)
  | "array"        { T_ARRAY }
  | "bool"         { T_BOOL  }
  | "float"        { T_FLOAT }
  | "int"          { T_INT }
  | "object"       { T_OBJECT }
  | "string"       { T_STRING }

(* keywords *)
  | "abstract"     { T_ABSTRACT }
  | "break"        { T_BREAK }
  | "case"         { T_CASE }
  | "catch"        { T_CATCH }
  | "class"        { T_CLASS }
  | "const"        { T_CONST }
  | "continue"     { T_CONTINUE }
  | "default"      { T_DEFAULT }
  | "else"         { T_ELSE }
  | "extends"      { T_EXTENDS }
  | "function"     { T_FUNCTION }
  | "if"           { T_IF }
  | "instanceof"   { T_INSTANCEOF }
  | "new"          { T_NEW }
  | ['N' 'n']['U' 'u']['L' 'l']['L' 'l']
                   { T_NULL }           (* TODO how to do case-insensitive matching? *)
  | "parent"       { T_PARENT }
  | "private"      { T_PRIVATE }
  | "protected"    { T_PROTECTED }
  | "public"       { T_PUBLIC }
  | "return"       { T_RETURN }
  | "self"         { T_SELF }
  | "static"       { T_STATIC }
  | "switch"       { T_SWITCH }
  | "this"         { T_THIS }
  | "throw"        { T_THROW }
  | "try"          { T_TRY }

(* various small things *)
  | '$'            { T_DOLLAR }
  | "->"           { T_RARROW }
  | "<<"           { T_LT_LT }
  | ">>"           { T_GT_GT }
  | "::"           { T_COLON_2 }
  | ':'            { T_COLON }
  | ';'            { T_SEMICOLON }
  | ','            { T_COMMA }
  | '?'            { T_QUESTION }

(* comparison operators *)
  | '<'            { T_LT }
  | "<="           { T_LT_EQ }
  | '>'            { T_GT }
  | ">="           { T_GT_EQ }
  | "=="           { T_EQ_EQ }
  | "==="          { T_EQ_EQ_EQ }
  | "!="           { T_EXCL_EQ }
  | "!=="          { T_EXCL_EQ_EQ }
  | "<>"           { T_LT_GT }

(* assignment operators *)
  | '='            { T_EQ }
  | "+="           { T_PLUS_EQ }
  | "-="           { T_MINUS_EQ }
  | "*="           { T_ASTERISK_EQ }
  | "/="           { T_SLASH_EQ }
  | ".="           { T_DOT_EQ }
  | "%="           { T_PERCENT_EQ }
  | "&="           { T_AMPERSAND_EQ }
  | "|="           { T_BAR_EQ }
  | "^="           { T_CARET_EQ }
  | "<<="          { T_LT_LT_EQ }
  | ">>="          { T_GT_GT_EQ }

(* string and arithmetic operators *)
  | '.'            { T_DOT }
  | '+'            { T_PLUS }
  | "++"           { T_PLUS_2 }
  | '-'            { T_MINUS }
  | "--"           { T_MINUS_2 }
  | '*'            { T_ASTERISK }
  | '/'            { T_SLASH }
  | '%'            { T_PERCENT }

(* brackets *)
  | '('            { T_LPAREN }
  | ')'            { T_RPAREN }
  | '['            { T_LBRACS }
  | ']'            { T_RBRACS }
  | '{'            { T_LBRACC }
  | '}'            { T_RBRACC }

(* comments *)
  | '#'            {  lineComment lexbuf }
  | "//"           {  lineComment lexbuf }
  | "/*"           { blockComment lexbuf }

(* other words *)
  | [' ' '\t']+    { token lexbuf }                             (* skip blanks *)
  | '\n'           { newlineUpdate lexbuf; token lexbuf }
  |  digit+ as lxm { T_INT_LITERAL(int_of_string lxm) }         (* TODO hex, oct etc. *)
  | (digit+ '.' digit+) as lxm
                   { T_FLOAT_LITERAL(float_of_string lxm) }
  | '\'' (('\"' | "\\'" | '\\' | string_char)* as lxm) '\''
                   { T_STRING_LITERAL_S(lxm) }
  | '\"' (('\'' | "\\\"" | "\\\\" | "\\n" | string_char)* as lxm) '\"'
                   { (*print_endline("StringDQ: " ^ lxm ^ "#");*) T_STRING_LITERAL_D(lxm) }
  | identifier as lxm
                   { T_IDENTIFIER(lxm) }
  | eof            { raise End_of_file }

and lineComment = parse
    '\n'           { newlineUpdate lexbuf; token lexbuf }
  | eof            { raise End_of_file }
  | _              { lineComment lexbuf }

and blockComment = parse
    "*/"           { token lexbuf }
  | '\n'           { newlineUpdate lexbuf; blockComment lexbuf }
  | eof            { raise End_of_file }
  | _              { blockComment lexbuf }
