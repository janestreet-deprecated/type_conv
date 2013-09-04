(** Pa_type_conv: Preprocessing Module for Registering Type Conversions *)

open Camlp4.PreCast.Ast

(** {6 Generator registration} *)

val set_conv_path_if_not_set : Loc.t -> unit
(** [set_conv_path_if_not_set loc] sets the path to the file/module being
    converted for improved error messages. *)

val get_conv_path : unit -> string
(** [get_conv_path ()] @return the name to module containing a type
    as required for error messages. *)

val add_generator : ?is_exn : bool -> string -> (bool -> ctyp -> str_item) -> unit
(** [add_generator ?is_exn name gen] adds the code generator [gen],
    which maps type or exception declarations to structure items, where
    [is_exn] specifies whether the declaration is an exception.  Note that
    the original type/exception declarations get added automatically in
    any case.

    @param is_exn = [false]
*)

val add_generator_with_arg :
  ?is_exn : bool -> string -> 'a Camlp4.PreCast.Gram.Entry.t ->
  ('a option -> bool -> ctyp -> str_item) -> unit
(** [add_generator_with_arg ?is_exn name entry generator] same as
    [add_generator], but the generator may accept an argument, which is
    parsed with [entry]. *)

val rm_generator : ?is_exn : bool -> string -> unit
(** [rm_generator ?is_exn name] removes the code generator named [name]
    for types if [is_exn] is [false], or exceptions otherwise.

    @param is_exn = [false]
*)

val add_sig_generator :
  ?delayed : bool -> ?is_exn : bool ->
  string -> (bool -> ctyp -> sig_item) -> unit
(** [add_sig_generator ?delayed ?is_exn name gen] adds the code generator [gen],
    which maps type or exception declarations to signature items, where
    [is_exn] specifies whether the declaration is an exception.  Note that the
    original type/exception declarations get added automatically in any case. If
    [delayed] is set to true, the output of this generator is appended to the
    signature in which it's defined

    @param delayed = [false]
    @param is_exn = [false]
*)

val add_sig_generator_with_arg :
  ?delayed : bool -> ?is_exn : bool -> string ->
  'a Camlp4.PreCast.Gram.Entry.t ->
  ('a option -> bool -> ctyp -> sig_item) -> unit
(** [add_sig_generator_with_arg ?delayed ?is_exn name entry generator] same as
    [add_sig_generator], but the generator may accept an argument,
    which is parsed with [entry]. *)

val rm_sig_generator : ?is_exn : bool -> string -> unit
(** [rm_sig_generator ?is_exn name] removes the signature code generator named
    [name] for types if [is_exn] is [false], or exceptions otherwise.

    @param is_exn = [false]
*)

(** Type of record field code generators *)
type record_field_generator = ctyp -> unit

val add_record_field_generator : string -> record_field_generator -> unit
(** [add_record_field_generator gen_name gen] adds the record field code
    generator [gen] with name [gen_name], which acts on the location
    identifiying the record field. *)

val add_record_field_generator_with_arg :
  string -> 'a Camlp4.PreCast.Gram.Entry.t ->
  ('a option -> record_field_generator) -> unit
(** [add_record_field_generator_with_arg name entry generator] same as
    [add_record_field_generator], but the [generator] takes an argument,
    which is parsed with [entry].  If [None] is passed to the generator,
    parsing of the argument failed, otherwise [Some arg] will be passed,
    where [arg] is the successfully parsed argument. *)

val rm_record_field_generator : string -> unit
(** [rm_record_field_generator name] removes the record field code generator
    named [name]. *)

(** {6 Generator sets registration} *)

val add_sig_set : ?is_exn: bool -> string -> set: string list -> unit
(** [add_sig_set ?is_exn id ~set] adds the generator [id] to the list
    of generators for signatures.
    This generator will behave as if is all the generators from [set]
    had been given instead. Any duplicate arising from repeatedly
    expanding such generators are removed.
    If [is_exn], then it is a generator for exception declaration, or
    else it is a generator for type declaration.
*)

val add_str_set : ?is_exn: bool -> string -> set: string list -> unit
(** [add_str_set ?is_exn id ~set] behaves exactly like
    [add_sig_set ?is_exn id ~set] but for structure items instead of
    signatures items.
*)

val add_set :
  kind:[`Str | `Sig | `Both] ->
  is_exn:[`Yes | `No | `Both] ->
  string ->
  set:string list ->
  unit
(** [add_set ~kind ~is_exn id ~set] is a shorthand for doing multiple
    calls to [add_str_set] and [add_sig_set]
*)

(** {6 Utility functions} *)

val get_loc_err : Loc.t -> string -> string
(** [get_loc_err loc msg] generates a compile-time error message. *)

val hash_variant : string -> int
(** [hash_variant str] @return the integer encoding a variant tag with
    name [str]. *)


(** {6 General purpose code generation module} *)

module Gen : sig

  val regular_constr_of_revised_constr : string -> string
    (* Transforms names of constructor of sum types (including polymorphic variants) from
       their revised representation in the camlp4 ast to the representation they would
       have in ocaml's ast.

       This is supposed to be used like this:
       match ctyp with
       | <:ctyp< $uid:constr$ >> ->
         <:expr< $str:regular_constr_of_revised_constr constr$ >>
       | _ -> ...

       so that <:ctyp< True >> becomes "true" and <:ctyp< True >> (assuming regular
       ocaml in the quotation) becomes "True" and not " True".

       Everything also applies to exception names. *)

  val exApp_of_list : expr list -> expr
  (** [expr_app_of_list l] takes list [l] of expressions [e1; e2; e3; ...]
      and returns the expression [e1 e2 e3].  C.f.: [Ast.exSem_of_list]. *)

  val tyArr_of_list : ctyp list -> ctyp
  (** [tyArr_of_list l] takes list [l] of types [e1; e2; e3; ...] and
      returns the type [e1 e2 e3].  C.f.: [Ast.exSem_of_list]. *)

  val paOr_of_list : patt list -> patt
  (** [paOr_of_list l] takes list [l] of patterns [p1; p2; p3; ...] and returns
      the pattern [p1 | p2 | p3 | ...] *)

  val gensym : ?prefix : string -> unit -> string
  (** [gensym ?prefix ()] generates a fresh variable name with [prefix].
      When used with the default parameters, it will return: [_x__001],
      [_x__002], [_x__003], ...

      @param prefix default = "_x"
  *)

  val error : ctyp -> fn : string -> msg : string -> _
  (** [error tp ~fn ~msg] raises an error with [msg] on type [tp] occuring
      in function [fn]. *)

  val unknown_type : ctyp -> string -> _
  (** [unknown_type tp fn] type [tp] cannot be handled by function [fn]. *)

  val ty_var_list_of_ctyp : ctyp -> string list -> string list
  (** [ty_var_list_of_ctyp tp acc] accumulates a list of type parameters
      contained in [tp] into [acc] as strings. *)

  val get_rev_id_path : ident -> string list -> string list
  (** [get_rev_id_path id acc] takes an identifier.  @return a reversed
      module path (list of strings) denoting this identifier, appending
      it to [acc]. *)

  val ident_of_rev_path : Loc.t -> string list -> ident
  (** [ident_of_rev_path loc path] takes a location [loc] and a reversed path
      [rev_path] to an identifier.  @return identifier denoting the
      bound value. *)

  val get_appl_path : Loc.t -> ctyp -> ident
  (** [get_appl_path loc tp] @return the identifier path associated with
      a polymorphic type. *)

  val abstract : Loc.t -> patt list -> expr -> expr
  (** [abstract loc patts body] takes a location [loc], a pattern list
      [patts], and an expression [body].  @return a function expression
      that takes the patterns as arguments, and binds them in [body]. *)

  val apply : Loc.t -> expr -> expr list -> expr
  (** [apply loc f_expr arg_exprs] takes a location [loc], an expression
      [f_expr] representing a function, and a list of argument expressions
      [arg_exprs].  @return an expression in which the function is
      applied to its arguments. *)

  val switch_tp_def :
    alias : (Loc.t -> ctyp -> 'a) ->
    sum : (Loc.t -> ctyp -> 'a) ->
    record : (Loc.t -> ctyp -> 'a) ->
    variants : (Loc.t -> ctyp -> 'a) ->
    mani : (Loc.t -> ctyp -> ctyp -> 'a) ->
    nil : (Loc.t -> 'a) ->
    ctyp
    -> 'a
  (** [switch_tp_def ~alias ~sum ~record ~variants ~mani tp_def]
      takes a handler function for each kind of type definition and
      applies the appropriate handler when [tp_def] matches. *)

  val mk_expr_lst : Loc.t -> expr list -> expr
  (** [mk_expr_lst loc expr_list] takes a list of expressions.
      @return an expression representing a list of expressions. *)

  val mk_patt_lst : Loc.t -> patt list -> patt
  (** [mk_patt_lst _loc patt_list] takes a list of patterns.
      @return a pattern representing a list of patterns. *)

  val get_tparam_id : ctyp -> string
  (** [get_tparam_id tp] @return the string identifier associated with
      [tp] if it is a type parameter.  @raise Failure otherwise. *)

  val type_is_recursive :
    ?stop_on_functions:bool ->
    ?short_circuit:(ctyp -> bool option) ->
    string -> ctyp -> bool
  (** [type_is_recursive ?short_circuit id tp]
      @return whether the type [tp] with name [id]
      refers to itself, assuming that it is not mutually recursive with
      another type.

      @param short_circuit allows you to override the search for certain
      type expressions.
      @param stop_on_functions allows to disregard the recursive occurences appearing in
      arrow types. The default is to disregard them.
  *)

  val drop_variance_annotations : ctyp -> ctyp
  (** [drop_variance_annotations tp] @return the type resulting from dropping
      all variance annotations in [tp]. *)

  val find_record_default : Loc.t -> expr option
  (** [find_record_default loc] @return the optional default expression
      associated with the record field at source location [loc] if defined. *)

  val delay_sig_item : sig_item -> unit
  (** [delay_sig_item item] places [item] at the end of the current signature *)
end

(** {6 Utility functions to rewrite type definitions} *)

module Rewrite_tds : sig
  val sig_ : Loc.t -> bool -> ctyp -> sig_item
  (** [sig_ loc rec_ typedefs] rewrites the given type definition to make it either
      recursive or non recursive.
      For instance, the parser calls [sig_ loc false (TyDcl (_, t, [], t, []))] when it
      encouters [type t = t] and calls [sig_ loc true (TyDcl (_, t, [], t, []))] when it
      encouters [type nonrec t = t] in signatures. *)

  val str_ : Loc.t -> bool -> ctyp -> str_item
  (** [str_ loc rec_ typedefs] does the same thing as [sig_ loc rec_ typedefs], except
      that it returns a structure item instead of a signature item. *)
end
