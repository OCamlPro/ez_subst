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


type 'context t = ('context -> string -> string)
type brace = [ `Brace | `Paren | `Bracket ]

exception UnclosedExpression of string

let buffer ?(sep = '$') ?(brace=`Brace) ?(sym=false) f b context s =
  let len = String.length s in
  let (boi, eoi) = match brace with
    | `Brace -> '{', '}'
    | `Paren -> '(', ')'
    | `Bracket -> '[', ']'
  in

  let rec iter b stack i = (* default state *)
    if i = len then
      match stack with
      | [] -> ()
      | _b :: _ ->
          raise (UnclosedExpression ( Buffer.contents b ))
    else
      let c = s.[i] in
      if c = sep then
        iter1 b stack (i+1)
      else
      if c = eoi then
        match stack with
          [] -> Buffer.add_char b c ; iter b stack (i+1)
        | b1 :: stack1 ->
            if sym then
              iter2 b stack (i+1)
            else
              let ident = Buffer.contents b in
              let replacement = f context ident in
              Buffer.add_string b1 replacement;
              iter b1 stack1 (i+1)
      else begin
        Buffer.add_char b c ;
        iter b stack (i+1)
      end

  and iter1 b stack i = (* found '$' *)
    if i = len then begin
      Buffer.add_char b sep;
      iter b stack i
    end
    else
      let c = s.[i] in
      if c = sep then begin
        Buffer.add_char b sep ;
        iter b stack (i+1)
      end else
      if c = boi then
        iter (Buffer.create 16) (b :: stack) (i+1)
      else begin
        Buffer.add_char b sep;
        Buffer.add_char b c;
        iter b stack (i+1)
      end

  and iter2 b stack i = (* stack<>[] & found '}', need '$' *)
    if i = len then
      raise (UnclosedExpression (Buffer.contents b))
    else
      let c = s.[i] in
      if c = sep then begin
        match stack with
        | [] -> assert false
        | b1 :: stack ->
            let ident = Buffer.contents b in
            let replacement = f context ident in
            Buffer.add_string b1 replacement;
            iter b1 stack (i+1)
      end
      else begin
        Buffer.add_char b eoi;
        iter b stack i
      end
  in
  iter b [] 0

let string ?sep ?brace ?sym f context s =
  let b = Buffer.create ( String.length s ) in
  buffer ?sep ?brace ?sym f b context s;
  Buffer.contents b
