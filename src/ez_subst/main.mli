(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2020 OCamlPro & Origin Labs                             *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(**************************************************************************)

(** Easy Substitutions in Strings

  [ez_subst] provides simple functions to perform substitutions
  of identifiers in strings. By default, identifiers are recognized as
  [${ident}], but the identifier syntax can be customized by:
  {ul
  {- changing the separator [sep] (default is ['$'])}
  {- changing the brace [brace] (default is [`Brace] for ['{'..'}'])}
  {- using a symmetric notation [sym] (default is [false], whereas [true]
     means ['${ident}$'].
  }
*)

type 'context t = ('context -> string -> string)
(** The type for functions performing the translation from [ident] to
    its replacement. ['context] is some information, that is from the
    initial call to the substitution. *)

type brace = [ `Brace | `Paren | `Bracket ]
(** The type to choose the type of parenthizing. *)

exception UnclosedExpression of string
(** The only exception that may be raised by substitutions: it indicates
    that the end of the expression could not be found. *)

val string : ?sep:char -> ?brace:brace -> ?sym:bool ->
  'context t -> 'context -> string -> string
(** [string f context s] performs substitutions on [s] following [f],
   passing the context [context] to [f] for every expression,
   returning the result as a string. *)

val buffer : ?sep:char -> ?brace:brace -> ?sym:bool ->
  'context t -> Buffer.t -> 'context -> string -> unit
(** [buffer f b context s] performs substitutions on [s] following [f],
   passing the context [context] to [f] for every expression,
   returning the result by appending it to the buffer [b]. *)
