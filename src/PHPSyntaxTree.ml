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

(* Token data: file name, line number, line offset, token *)
type tokenData = TokenData of (string * int * int);;

type constantLiteral = Null     of (tokenData)
                     | Int      of (tokenData * int)
                     | Float    of (tokenData * float)
                     | StringSQ of (tokenData * string);;

type simpleType = TypeBool   of tokenData
                | TypeFloat  of tokenData
                | TypeInt    of tokenData
                | TypeString of tokenData;;

type identifier = Identifier of (tokenData * string);;
type   variable = This     of tokenData
                | Variable of (tokenData * string);;

type identifier_variable = IdentVar_ident of identifier
                         | IdentVar_var   of variable;;

type expression =
    ConstLiteral of constantLiteral
  | StringDQ of (tokenData * string)
  |   VarExpr of variable
  | IdentExpr of identifier
  | New of (identifier * expression list)
  | Dereference of (expression * identifier_variable list) (* (f()[5])->$a->b->$c *)
  | Parent of (identifier_variable list)
  | Self   of (identifier_variable list)
  | StaticReferenceChain of (identifier * identifier_variable list) (* A::$a->b->$c *)
  | ArrayExpr of (expression * expression)
  | FunCallExpr of (expression * expression list)
  | PreDecrement of expression
  | PreIncrement of expression
  | PostDecrement of expression
  | PostIncrement of expression
  | UnaryMinus of expression
  | BitwiseNot of expression
  | TypeCast of (simpleType * expression)
  | InstanceOf of (expression * identifier)
  | LogicalNot of expression
  | Multiplication of (expression * expression)
  | Division       of (expression * expression)
  | Modulo         of (expression * expression)
  | Plus   of (expression * expression)
  | Minus  of (expression * expression)
  | Concat of (expression * expression)
  | ShiftLeft  of (expression * expression)
  | ShiftRight of (expression * expression)
  | IsSmaller   of (expression * expression)
  | IsSmallerEq of (expression * expression)
  | IsEqual of (expression * expression)
  | IsIdentical of (expression * expression)
  | BitwiseAnd of (expression * expression)
  | BitwiseXor of (expression * expression)
  | BitwiseOr  of (expression * expression)
  | LogicalAnd of (expression * expression)
  | LogicalXor of (expression * expression)
  | LogicalOr  of (expression * expression)
  | TernaryChoice of (expression * expression * expression)
  | AssignExpr of (expression * expression);;

type formalArgument =      FormalArg of variable
                    | TypedFormalArg of (identifier * variable);;
type formalArgumentWDefault = FormalArgWDefault of (formalArgument * constantLiteral);;
type formalArgsList = (formalArgument list * formalArgumentWDefault list);;

type switchItem = SwCase of expression
                | SwDefault;;

type variableDeclaration = VarDecl      of  variable
                         | VarDeclAssig of (variable * expression);;

type statement =
    AssignStmt of (expression * expression)
  | VarDeclStmt of variableDeclaration
  | FunCallStmt of (expression * expression list)
  | Break of tokenData
  | Return of expression
  | Throw of expression
  | If     of (expression * statement)
  | IfElse of (expression * statement * statement)
  | SwitchStmt of (expression * (switchItem * statement) list)
  | TryCatch of (statement * (formalArgument * statement) list)              (* Must be >=1 catch clauses! *)
  | BlockStmt of (statement list);;

type functionDefinition = Function of (identifier * formalArgsList * statement);;

type classItemVisibility = Public | Protected | Private;;
type classItem =
    InstanceMethod of (classItemVisibility * functionDefinition)
  |   StaticMethod of (classItemVisibility * functionDefinition)
  |   StaticVar    of (classItemVisibility * variableDeclaration)
  |   StaticConst  of (classItemVisibility * identifier * expression);;

type abstractClause = Concrete | Abstract;;
type  extendsClause = RootClass | Extends of identifier;;
type classDefinition = Class of (abstractClause * identifier * extendsClause * classItem list);;

type sourceFileItem = PHPStatement of statement
                    | PHPFunction  of functionDefinition
                    | PHPClass     of classDefinition;;
type sourceFile = PHPSourceFile of sourceFileItem list;;

let funCallExprAsPair fce =
    match fce with
        FunCallExpr(f,params) -> (f,params)
      | _                     -> raise (PHPAnalException("expected FunCallExpr"))
;;
