(* chk-pat.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *)

structure ChkPat : sig

    (* type check a pattern with respect to the given type.  Return the AST
     * equivalent pattern.
     *
     * Note: the specified type of the pattern might just be a meta variable
     * (e.g., when the pattern is a function parameter), but when it has more
     * information we can check that against the structure of the pattern.
     *)
    val check : Context.t * Type.ty * BindTree.pat -> AST.pat

  end = struct

    structure BT = BindTree
    structure C = Context
    structure Ty = Type
    structure TU = TypeUtil
    structure U = Unify

    (* placeholder pattern for when there is an error *)
    val bogusPat = AST.TuplePat[]

    fun check (cxt, ty, BT.MarkPat m) : AST.pat = let
          val (cxt, pat) = C.withMark (cxt, m)
          in
            check (cxt, ty, pat)
          end
      | check _ = raise Fail "FIXME"

  end
