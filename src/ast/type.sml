(* type.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Concrete representation of MiniML types for the type checker.
 *)

structure Type : sig

    (* bound type variables *)
    type tyvar = TyVar.t

    (* types *)
    datatype ty
      = VarTy of tyvar		        (* bound type variable *)
      | MetaTy of meta_var	        (* unification meta variable *)
      | ConTy of tycon * ty list        (* type constructor application; includes function
                                         * and tuple types.
                                         *)
      | ErrorTy			        (* unknown type used to avoid cascading errors *)

    (* unification meta variables *)
    and meta_var = MV of {
        id : Stamp.t,                   (* unique ID *)
        info : meta_info ref            (* either the instantiation of the variable or
                                         * meta-data about the variable.
                                         *)
      }

    and meta_info
      = UNIV of int                     (* variable introduced at given depth *)
      | INST of ty                      (* substitution instance *)

    (* type schemes *)
    and scheme = TyScm of tyvar list * ty

    (* a type constructor (primitive or introduced by a `type` declaration) *)
    and tycon
      = PrimTyc of {            (* built-in type constructors *)
            name : string,        (* the type's name *)
            id : Stamp.t,         (* unique stamp *)
            params : tyvar list   (* bound type parameters *)
          }
      | FunTyc                  (* '->' type constructor *)
      | TupleTyc of int         (* tuple type constructor; integer is arity (!= 1) *)
      | UserTyc of usr_tyc      (* user-declared type *)

    (* a data constructor *)
    and dcon = DCon of {        (* data constructor *)
        name : string,          (* constructor name *)
        id : Stamp.t,           (* unique identifier *)
        argTys : ty list,       (* argument types; `[]` for nullary constructors *)
        dty : usr_tyc           (* the type constructor that this dcon belongs to *)
      }

    withtype usr_tyc = {
          name : string,        (* the type's name *)
          id : Stamp.t,         (* unique stamp *)
          params : tyvar list,  (* bound type parameters *)
          cons : dcon list ref  (* list of data constructors *)
        }

    (* primitive type constructors *)
    val unitTyc : tycon
    val intTyc : tycon
    val stringTyc : tycon

    (* create a fresh meta-variable type at the given nesting depth *)
    val metaVarTy : int -> ty

    (* make a function type *)
    val funTy : ty * ty -> ty

    (* `asFunTy` returns `SOME(ty1, ty2)` when `ty` is a function type from `ty`
     * to `ty2`; otherwise it returns `NONE`.
     *)
    val asFunTy : ty -> (ty * ty) option

    (* make a tuple type from type arguments; this function behaves as the identity
     * when the list has length one.
     *)
    val tupleTy : ty list -> ty

    (* if a type is an instantiated meta variable, the return the instantiation;
     * otherwise return the argument.
     *)
    val prune : ty -> ty

  end = struct

    open TypeRep

    val unitTyc = TyCon.newPrim(BindBasis.tycUnit, [])
    val intTyc = TyCon.newPrim(BindBasis.tycInt, [])
    val stringTyc = TyCon.newPrim(BindBasis.tycString, [])

    (* NOTE: prune is inherited from TypeRep *)

    fun metaVarTy d = MetaTy(MetaVar.fresh d)

    fun funTy (ty1, ty2) = ConTy(FunTyc, [ty1, ty2])

    fun asFunTy (ConTy(FunTyc, [ty1, ty2])) = SOME(ty1, ty2)
      | asFunTy _ = NONE

    fun tupleTy [] = ConTy(unitTyc, [])
      | tupleTy [ty] = ty
      | tupleTy tys = ConTy(TupleTyc(List.length tys), tys)

  end
