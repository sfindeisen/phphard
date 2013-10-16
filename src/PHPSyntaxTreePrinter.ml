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

open PHPSyntaxTree;;

let listToString lfun ll sepa =
    List.fold_left (fun s item -> (if (1 <= (String.length s)) then (s^sepa) else "") ^ (lfun item)) "" ll
;;

let toString_simpleType st =
    match st with
      TypeBool(_)   -> "bool"
    | TypeFloat(_)  -> "float"
    | TypeInt(_)    -> "int"
    | TypeString(_) -> "string"
;;

let toString_constantLiteral cl =
    match cl with
      Null(_)       -> "Null"
    | Int(_,i)      -> (string_of_int i)
    | Float(_,f)    -> (string_of_float f)
    | StringSQ(_,s) -> "\'" ^ s ^ "\'"
;;

let toString_identifier i =
    match i with
      Identifier(_,s) -> s
;;

let toString_variable v =
    match v with
      This(_)       -> "$this"
    | Variable(_,s) -> ("$" ^ s)
;;

let toString_formalArgument fa =
    match fa with
           FormalArg(v)   -> toString_variable(v)
    | TypedFormalArg(i,v) -> toString_identifier(i) ^ " " ^ toString_variable(v)
;;
let toString_formalArgumentDefault (FormalArgWDefault(fa, clit)) =
    (toString_formalArgument fa ^ " = " ^ (toString_constantLiteral clit))
;;
let toString_formalArgument_list falist =
    listToString toString_formalArgument falist ", "
;;
let toString_formalArgumentDefault_list fawdlist =
    listToString toString_formalArgumentDefault fawdlist ", "
;;

let toString_formalArgs_list2 (falist, fawdlist) =
    let   falistStr = toString_formalArgument_list          falist
    and fawdlistStr = toString_formalArgumentDefault_list fawdlist
    in
        let sepa = (if ((1 <= (String.length falistStr)) && (1 <= (String.length fawdlistStr))) then ", " else "")
        in
            falistStr ^ sepa ^ fawdlistStr
;;

let toString_identifier_variable iv =
    match iv with
      IdentVar_ident(i) -> toString_identifier(i)
    | IdentVar_var(v)   -> toString_variable(v)
;;

let toString_identifier_variable_list ivlist =
    listToString toString_identifier_variable ivlist "->"
;;

(* comma separated expression list *)
let toString_expression_list toString_expr el =
    listToString toString_expr el ", "
;;

(**
 * Pretty printing: we try to use as few () as possible.
 *)
let toString_expression expr =
    let rec exprStr precReq expr =
        let par precActual s =
            (if (precActual < precReq) then "(" ^ s ^ ")" else s)
        in
            let unOpStrPre e ops precActual isRightAssoc =
                let precReqRight = precActual + (if isRightAssoc then 0 else 1)
                in
                    par precActual ops ^ (exprStr precReqRight e);
            and unOpStrPost e ops precActual isLeftAssoc =
                let precReqLeft = precActual + (if isLeftAssoc then 0 else 1)
                in
                    par precActual (exprStr precReqLeft e) ^ ops;
            and binOpStr e1 e2 ops precActual isLeftAssoc isRightAssoc =
                let precReqLeft  = precActual + (if isLeftAssoc  then 0 else 1)
                and precReqRight = precActual + (if isRightAssoc then 0 else 1)
                in
                    par precActual ((exprStr precReqLeft e1) ^ " " ^ ops ^ " " ^ (exprStr precReqRight e2))
            in

    match expr with
      ConstLiteral(c)    -> toString_constantLiteral(c)
    | StringDQ(_,s)      -> ("\"" ^ s ^ "\"")
    |   VarExpr(v)       -> toString_variable(v)
    | IdentExpr(i)       -> toString_identifier(i)

    | New(i,el) -> par 24 ("new " ^ toString_identifier(i) ^ "(" ^ (toString_expression_list (exprStr 0) el) ^ ")")
    | Dereference(e, [])             -> par 23 (exprStr precReq e)
    | Dereference(e, h::t)           -> par 23 ((exprStr 23 e) ^ "->" ^ toString_identifier_variable_list(h::t))
    | Parent(ivlist)                 -> par 22 ("parent::" ^ toString_identifier_variable_list(ivlist))
    | Self(ivlist)                   -> par 22 (  "self::" ^ toString_identifier_variable_list(ivlist))
    | StaticReferenceChain(i, [])    -> toString_identifier(i)
    | StaticReferenceChain(i, ivlist)-> par 22 (toString_identifier(i)  ^ "::" ^ toString_identifier_variable_list(ivlist))
    | ArrayExpr(e1,e2)               -> par 21 ((exprStr 21 e1) ^ "[" ^ (exprStr 0 e2) ^ "]")
    | FunCallExpr(f, al)             -> par 20 ((exprStr 21 f)  ^ "(" ^ (toString_expression_list (exprStr 0) al) ^ ")")

    | PreDecrement(e)  -> (unOpStrPre  e "--" 19 false)
    | PreIncrement(e)  -> (unOpStrPre  e "++" 19 false)
    | PostDecrement(e) -> (unOpStrPost e "--" 19 false)
    | PostIncrement(e) -> (unOpStrPost e "++" 19 false)
    | UnaryMinus(e)    -> (unOpStrPre  e "-"  18 false)
    | BitwiseNot(e)    -> (unOpStrPre  e "~"  18 false)
    | TypeCast(s,e)    -> par 18 ("(" ^ toString_simpleType(s) ^ ") " ^ (exprStr 19 e))
    | InstanceOf(e,i)  -> par 17 ((exprStr 18 e) ^ " instanceof " ^ toString_identifier(i))
    | LogicalNot(e)    -> (unOpStrPre  e "!"  16 true)

    | Multiplication (e1,e2) -> (binOpStr e1 e2 "*"   15 true  true)
    | Division       (e1,e2) -> (binOpStr e1 e2 "/"   15 true  false)
    | Modulo         (e1,e2) -> (binOpStr e1 e2 "%"   15 true  false)
    | Plus           (e1,e2) -> (binOpStr e1 e2 "+"   14 true  true)
    | Minus          (e1,e2) -> (binOpStr e1 e2 "-"   14 true  false)
    | Concat         (e1,e2) -> (binOpStr e1 e2 "."   14 true  true)
    | ShiftLeft      (e1,e2) -> (binOpStr e1 e2 "<<"  13 true  false)
    | ShiftRight     (e1,e2) -> (binOpStr e1 e2 ">>"  13 true  false)
    | IsSmaller      (e1,e2) -> (binOpStr e1 e2 "<"   12 false false)
    | IsSmallerEq    (e1,e2) -> (binOpStr e1 e2 "<="  12 false false)
    | IsEqual        (e1,e2) -> (binOpStr e1 e2 "=="  11 false false)
    | IsIdentical    (e1,e2) -> (binOpStr e1 e2 "===" 11 false false)
    | BitwiseAnd     (e1,e2) -> (binOpStr e1 e2 "&"   10 true  true)
    | BitwiseXor     (e1,e2) -> (binOpStr e1 e2 "^"    9 true  true)
    | BitwiseOr      (e1,e2) -> (binOpStr e1 e2 "|"    8 true  true)
    | LogicalAnd     (e1,e2) -> (binOpStr e1 e2 "&&"   7 true  true)
    | LogicalOr      (e1,e2) -> (binOpStr e1 e2 "||"   6 true  true)
    | TernaryChoice(e1,e2,e3)-> par 5 ((exprStr 6 e1) ^ " ? " ^ (exprStr 6 e2) ^ " : " ^ (exprStr 6 e3))
    | AssignExpr     (e1,e2) -> par 4 ((exprStr 5 e1) ^ " = " ^ (exprStr 5 e2))
    | LogicalXor     (e1,e2) -> (binOpStr e1 e2 "xor"  2 true  true)
in
    exprStr 0 expr
;;

let toString_expression_list =
    toString_expression_list toString_expression
;;

let toString_variableDeclaration vd =
    match vd with
      VarDecl     (v)   -> toString_variable(v)
    | VarDeclAssig(v,e) -> toString_variable(v) ^ " = " ^ (toString_expression e)
;;

let toString_catch_list toString_stmt fasl iakk i =
    listToString (function (fa,s) -> "catch (" ^ toString_formalArgument(fa) ^ ") " ^ (toString_stmt s (iakk^i) i)) fasl ""
;;

let toString_switchItem_list toString_stmt sisl iakk i =
    let itemString (c, s) =
        let stmtStr = toString_stmt s (iakk^i) i
        in
            match c with
               SwCase(e) -> iakk ^ "case (" ^ (toString_expression e) ^ "):\n" ^ stmtStr
            |  SwDefault -> iakk ^ "default:\n" ^ stmtStr
    in
        listToString itemString sisl ""
;;

let toString_statement_list toString_stmt sl iakk i =
    listToString (function s -> toString_stmt s iakk i) sl ""
;;

let rec toString_statement stmt iakk i =
    match stmt with
      AssignStmt(e1,e2) -> (iakk ^ (toString_expression e1) ^ " = " ^ (toString_expression e2) ^ ";\n")
    | VarDeclStmt(vd)   -> (iakk ^ toString_variableDeclaration(vd) ^ ";\n")
    | FunCallStmt(f,al) -> (iakk ^ (toString_expression f) ^ "(" ^ (toString_expression_list al) ^ ");\n")
    | Break(_)          -> "break;"
    | Return(e)         -> (iakk ^ "return " ^ (toString_expression e) ^ ";\n")
    | Throw(e)          -> (iakk ^ "throw "  ^ (toString_expression e) ^ ";\n")
    | If(e,s)           -> (iakk ^ "if ("  ^ (toString_expression e) ^ ")\n" ^ (toString_statement s  (iakk^i) i))
    | IfElse(e,s1,s2)   -> (iakk ^ "if ("  ^ (toString_expression e) ^ ")\n" ^ (toString_statement s1 (iakk^i) i) ^ iakk ^ "else\n" ^ (toString_statement s2 (iakk^i) i))
    | SwitchStmt(e,sisl)-> (iakk ^ "switch (" ^ (toString_expression e) ^ ") {\n" ^ (toString_switchItem_list toString_statement sisl (iakk^i) i) ^ "}\n")
    | TryCatch(s,fasl)  -> (iakk ^ "try {\n" ^ (toString_statement s (iakk^i) i) ^ (toString_catch_list toString_statement fasl (iakk^i) i) ^ "\n")
    | BlockStmt(sl)     -> (iakk ^ "{\n" ^ (toString_statement_list toString_statement sl (iakk^i) i) ^ iakk ^ "}\n")
;;

let toString_statement_list =
    toString_statement_list toString_statement
;;

let toString_function_basic (Function(id, falist, stmt)) indStart indAkk ind =
    indStart ^ "function " ^ (toString_identifier id) ^ "(" ^ (toString_formalArgs_list2 falist) ^ ")\n" ^ (toString_statement stmt indAkk ind)
;;
let toString_function f indAkk ind =
    toString_function_basic f indAkk indAkk ind
;;

let toString_class (Class(abstractDef,classId,extendsDef,items)) indAkk ind =
    let toString_classItem_list items indAkk ind =
        let visStr vis =
            match vis with
            Public    -> "public"
            | Protected -> "protected"
            | Private   -> "private"
        in
            let itemStr it =
                match it with
                InstanceMethod(vis, funDef)  -> indAkk ^ (visStr vis) ^ " "        ^ (toString_function_basic funDef "" indAkk ind)
                |   StaticMethod(vis, funDef)  -> indAkk ^ (visStr vis) ^ " static " ^ (toString_function_basic funDef "" indAkk ind)
                |   StaticVar   (vis, varDecl) -> indAkk ^ (visStr vis) ^ " static " ^ (toString_variableDeclaration varDecl)
                |   StaticConst (vis, constId, constExpr) -> indAkk ^ (visStr vis) ^ " const " ^ (toString_identifier constId) ^ " = " ^ (toString_expression constExpr)
            in
                List.fold_left (fun s item -> s ^ (itemStr item) ^ "\n") "" items
    in
        let abstractStr = match abstractDef with Abstract -> "abstract " | Concrete -> ""
        and  extendsStr = match  extendsDef with RootClass -> "" | Extends(ie) -> (" extends " ^ toString_identifier(ie))
        in
            indAkk ^ abstractStr ^ "class " ^ (toString_identifier classId) ^ extendsStr ^ "\n" ^ indAkk ^ "{\n" ^ (toString_classItem_list items (indAkk^ind) ind) ^ indAkk ^ "}\n"
;;

let toString_PHPSourceFile (PHPSourceFile(items)) =
    let phpItemStr indAkk ind it =
        match it with
          PHPStatement(stmt) -> (toString_statement stmt indAkk ind)
        | PHPFunction (fund) -> (toString_function  fund indAkk ind) ^ "\n"
        | PHPClass    (clsd) -> (toString_class     clsd indAkk ind) ^ "\n"
    in
        listToString (phpItemStr "" "    ") items ""
;;
