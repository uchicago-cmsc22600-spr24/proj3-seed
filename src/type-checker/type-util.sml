(* type-util.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Various operations on types used in type inference.
 *)

structure TypeUtil : sig

    (* apply a type variable to type substitution to a type.  The substitution
     * is represented as a list of type variable/type pairs.
     *)
    val substitute : (Type.ty * (Type.tyvar * Type.ty) list) -> Type.ty

    (* instantiate a type scheme with fresh meta variables at the specified depth *)
    val instantiate : Type.scheme * int -> Type.ty

    (* `instantiateTys (tvs, tys, depth)` instantiates the types `tys` by
     * replacing the type variables `tvs` with fresh meta variables at the
     * specified depth.
     *)
    val instantiateTys : Type.tyvar list * Type.ty list * int -> Type.ty list

    (* close a type w.r.t. to a set of non-generic variables (i.e., those
     * variables whose depth is less than or equal to the given depth).
     *)
    val closeTy : (Type.ty * int) -> Type.scheme

    (* return true if two types are equal (or one is an ErrorTy).  This function
     * does not do unification or alpha renaming of meta variables, but it does
     * chase instantiated meta-variable types.
     *)
    val same : (Type.ty * Type.ty) -> bool

    (* return the string representation of a type (for debugging) *)
    val toString : Type.ty -> string

  end = struct

    structure MV = MetaVar
    structure TVMap = TyVar.Map
    structure MVMap = MetaVar.Map
    structure Ty = Type

    (* helper function that applies a variable to type map to a type *)
    fun applySubst (subst, ty) = let
	  fun info ty = (case Ty.prune ty
		 of Ty.MetaTy _ => ty
		  | Ty.VarTy tv => TVMap.lookup(subst, tv)
		  | Ty.ConTy(tyc, args) => Ty.ConTy(tyc, List.map info args)
                  | Ty.ErrorTy => Ty.ErrorTy
		(* end case *))
	  in
	    info ty
	  end

    fun substitute (ty, []) = ty
      | substitute (ty, s) = applySubst (List.foldl TVMap.insert' TVMap.empty s, ty)

    fun instantiate (Ty.TyScm([], ty), _) = ty
      | instantiate (Ty.TyScm(tvs, ty), depth) = let
	  (* create a substitution from type variables to fresh meta variables *)
	  val subst = List.foldl
		(fn (tv, s) => TVMap.insert(s, tv, Ty.metaVarTy depth))
		  TVMap.empty tvs
	  in
	    applySubst (subst, ty)
	  end

    fun instantiateTys ([], tys, _) = tys
      | instantiateTys (tvs, tys, depth) = let
	  (* create a substitution from type variables to fresh meta variables *)
	  val subst = List.foldl
		(fn (tv, s) => TVMap.insert(s, tv, Ty.metaVarTy depth))
		  TVMap.empty tvs
	  in
	    List.map (fn ty => applySubst (subst, ty)) tys
	  end

    fun closeTy (ty, depth) = let
          val tvs = ref []
	  (* generate a fresh type variable *)
	  fun newVar () = let
                val tv = TyVar.fresh()
		in
		  tvs := tv :: !tvs;
                  tv
		end
          (* determine the generic variables in the type *)
	  fun genVars (ty, env) = (case Ty.prune ty
		 of ty as Ty.MetaTy(mv as Ty.MV{info=ref(Ty.UNIV d), ...}) =>
		      if (d > depth)
			then (case MVMap.find(env, mv) (* generic variable *)
			   of SOME tv => (env, Ty.VarTy tv)
			    | NONE => let
				val tv = newVar()
				in
				  (MVMap.insert(env, mv, tv), Ty.VarTy tv)
				end
			  (* end case *))
			else (env, ty) (* non-generic variable *)
		  | Ty.MetaTy _ => raise Fail "impossible"
		  | Ty.VarTy _ => raise Fail "unexpected type variable"
		  | Ty.ConTy(tyc, args) => let
		      val (env, tys) = genVarsForTys (args, env)
		      in
			(env, Ty.ConTy(tyc, tys))
		      end
                  | Ty.ErrorTy => (env, Ty.ErrorTy)
		(* end case *))
	  and genVarsForTys (tys, env) = let
		fun f (ty, (env, tys)) = let
			val (env', ty') = genVars(ty, env)
			in
			  (env', ty'::tys)
			end
		in
		  List.foldr f (env, []) tys
		end
	  val (_, ty) = genVars (ty, MVMap.empty)
	  in
	    Ty.TyScm(List.rev(!tvs), ty)
	  end

    fun same (ty1, ty2) = (case (Ty.prune ty1, Ty.prune ty2)
	   of (Ty.MetaTy mv1, Ty.MetaTy mv2) => MV.same(mv1, mv2)
	    | (Ty.VarTy tv1, Ty.VarTy tv2) => TyVar.same(tv1, tv2)
	    | (Ty.ConTy(tyc1, args1), Ty.ConTy(tyc2, args2)) =>
		TyCon.same(tyc1, tyc2) andalso ListPair.allEq same (args1, args2)
            | (Ty.ErrorTy, _) => true
            | (_, Ty.ErrorTy) => true
	    | _ => false
	  (* end case *))

    fun toString (Ty.MetaTy(Ty.MV{id, info})) = (case !info
	   of Ty.UNIV d => concat["$", Stamp.toString id, "@", Int.toString d]
	    | Ty.INST ty => (
		info := Ty.UNIV(~1);
		concat["$", Stamp.toString id, " == ", toString ty]
		  before info := Ty.INST ty)
	  (* end case *))
      | toString (Ty.VarTy tv) = TyVar.nameOf tv
      | toString (Ty.ConTy(tyc, [])) = TyCon.nameOf tyc
      | toString (Ty.ConTy(Ty.FunTyc, [ty1, ty2])) =
          concat[toString ty1, " -> ", toString ty2]
      | toString (Ty.ConTy(Ty.TupleTyc _, tys)) = let
          fun paren (ty as Ty.ConTy(Ty.FunTyc, _)) = concat["(", toString ty, ")"]
            | paren (ty as Ty.ConTy(Ty.TupleTyc _, _)) = concat["(", toString ty, ")"]
            | paren ty = toString ty
          in
            String.concatWithMap " * " paren tys
          end
      | toString (Ty.ConTy(tyc, tys)) = concat[
	    TyCon.nameOf tyc, "[", String.concatWithMap "," toString tys, "]"
	  ]
      | toString Ty.ErrorTy = "<error>"

  end
