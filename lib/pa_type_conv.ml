(* Pa_type_conv: Preprocessing Module for Registering Type Conversions *)

open Printf

open Camlp4
open PreCast
open Ast

(* Utility functions *)

let get_loc_err loc msg =
  sprintf "File \"%s\", line %d, characters %d-%d: %s"
    (Loc.file_name loc) (Loc.start_line loc)
    (Loc.start_off loc - Loc.start_bol loc)
    (Loc.stop_off loc - Loc.stop_bol loc)
    msg

(* To be deleted once the OCaml team fixes Mantis issue #4751.
   This function is copied from the compiler, function hash_variant
   in typing/btype.ml. *)
let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

let () = assert (Obj.magic `Latency_stats = hash_variant "Latency_stats")

(* Module/File path management *)

type path =
  | Not_initialized  (* Initial state *)
  | Too_late  (* already in a submodule, too late to initialize *)
  | Path of string * string list  (* Actually initialized *)

(* Reference storing the path to the currently preprocessed module *)
let conv_path_ref = ref Not_initialized

let get_conv_path_el () =
  match !conv_path_ref with
  | Path (e, el) -> e, el
  | _ -> failwith "Pa_type_conv: path not set"

(* Get path to the currently preprocessed module *)
let get_conv_path () = fst (get_conv_path_el ())

(* Set path to the currently preprocessed module *)
let set_conv_path conv_path =
  if !conv_path_ref = Not_initialized || !Sys.interactive then
    conv_path_ref := Path (conv_path, [conv_path])
  else failwith "Pa_type_conv: module name set twice"

let () = if !Sys.interactive then set_conv_path "Toplevel"

let push_conv_path mod_name =
  match !conv_path_ref with
  | Not_initialized -> conv_path_ref := Too_late (* Entered a submodule *)
  | Too_late -> ()
  | Path (str, rev_lst) ->
      conv_path_ref := Path (str ^ "." ^ mod_name, mod_name :: rev_lst)

let pop_conv_path () =
  match !conv_path_ref with
  | Path (_, _ :: rev_lst) ->
      conv_path_ref := Path (String.concat "." (List.rev rev_lst), rev_lst)
  | _ -> ()


module Signature_stack = struct
  module Item = struct
    type t = Ast.sig_item list ref

    let create () = ref []

    let delayed_sigs t      = List.rev !t
    let delay_sig    t item = t := item :: !t
  end

  let bottom : Item.t = Item.create ()
  let stack : Item.t list ref = ref [bottom]

  let push () =
    stack := Item.create () :: !stack

  let pop () =
    match !stack with
    | [] -> failwith "BUG: signature stack is empty"
    | top :: rest -> stack := rest; top

  let top () =
    match !stack with
    | [] -> failwith "BUG: signature stack is empty"
    | top :: _ -> top
end

(* Generator registration *)

type 'str_or_sig generator =
[ `Actual_generator of
    (Gram.Token.t * Syntax.Gram.token_info) list option ->
    bool ->
    Syntax.Ast.ctyp ->
    'str_or_sig
| `Set of string list ]

(* Map of "with"-generators for types in structures *)
let generators : (string, Ast.str_item generator) Hashtbl.t = Hashtbl.create 0

(* Map of "with"-generators for types in signatures *)
let sig_generators : (string, Ast.sig_item generator) Hashtbl.t = Hashtbl.create 0

(* Map of "with"-generators for exceptions in structures *)
let exn_generators : (string, Ast.str_item generator) Hashtbl.t = Hashtbl.create 0

(* Map of "with"-generators for exceptions in signatures *)
let sig_exn_generators : (string, Ast.sig_item generator) Hashtbl.t = Hashtbl.create 0

(* Map of "with"-generators for record fields *)
type record_field_generator = Ast.ctyp -> unit
let record_field_generators : (string, unit generator) Hashtbl.t = Hashtbl.create 0

(* Check that there is no argument for generators that do not expect any *)
let no_arg id e arg =
  if arg = None then e
  else
    failwith (
      "Pa_type_conv: generator '" ^ id ^ "' does not expect an argument")

(* Parse a list of tokens with the given grammar entry *)
let parse_with entry = function
  | Some tokens ->
      Some (Gram.parse_tokens_after_filter entry (Stream.of_list tokens))
  | None -> None

(* Entry which ignores its input *)
let ignore_tokens = Gram.Entry.of_parser "ignore_tokens" ignore

let make_generator entry e =
  `Actual_generator (fun arg rec_ typ -> e (parse_with entry arg) rec_ typ)

(* Add new generator, fail if already defined *)
let safe_add_gen gens id gen_or_set =
  if Hashtbl.mem gens id then
    failwith ("Pa_type_conv: generator '" ^ id ^ "' defined multiple times")
  else Hashtbl.add gens id gen_or_set

(* Register a "with"-generator for types in structures *)
let add_generator_with_arg ?(is_exn = false) id entry e =
  let gens = if is_exn then exn_generators else generators in
  safe_add_gen gens id (make_generator entry e)

let add_generator ?is_exn id e =
  add_generator_with_arg ?is_exn id ignore_tokens (no_arg id e)

(* Remove a "with"-generator for types in structures *)
let rm_generator ?(is_exn = false) id =
  let gens = if is_exn then exn_generators else generators in
  Hashtbl.remove gens id

(* Register a "with"-generator for types in signatures *)
let add_sig_generator_with_arg ?(delayed = false) ?(is_exn = false) id entry e =
  let e =
    if not delayed then e
    else fun arg rec_ tds ->
      Signature_stack.Item.delay_sig
        (Signature_stack.top ())
        (e arg rec_ tds);
      Ast.SgNil Loc.ghost
  in
  let gens = if is_exn then sig_exn_generators else sig_generators in
  safe_add_gen gens id (make_generator entry e)

let add_sig_generator ?delayed ?is_exn id e =
  add_sig_generator_with_arg ?delayed ?is_exn id ignore_tokens (no_arg id e)

(* Remove a "with"-generator for types in signatures *)
let rm_sig_generator ?(is_exn = false) id =
  let gens = if is_exn then sig_exn_generators else sig_generators in
  Hashtbl.remove gens id

(* Register a "with"-generator for record fields *)
let add_record_field_generator_with_arg id entry e =
  let e arg _rec tp = e arg tp in
  safe_add_gen record_field_generators id (make_generator entry e)

let add_record_field_generator id e =
  add_record_field_generator_with_arg id ignore_tokens (no_arg id e)

(* Remove a "with"-generator for record fields *)
let rm_record_field_generator id = Hashtbl.remove record_field_generators id

let add_set_with_tbl ~tbl ~id ~set ~descr =
  if List.mem id set then
    failwith (Printf.sprintf "Set of generator %s for %s is recursive" id descr);

  try
    let absent = List.find (fun id -> not (Hashtbl.mem tbl id)) set in
    failwith (
      sprintf "Set of generator %s for %s contains the generator %s, which is undefined"
        id descr absent
    )
  with Not_found ->
    safe_add_gen tbl id (`Set set)

let add_sig_set ?(is_exn = false) id ~set =
  let tbl = if is_exn then sig_exn_generators else sig_generators in
  let descr = if is_exn then "exceptions in signature items" else "types in signature items" in
  add_set_with_tbl ~tbl ~id ~set ~descr

let add_str_set ?(is_exn = false) id ~set =
  let tbl = if is_exn then exn_generators else generators in
  let descr = if is_exn then "exceptions in structure items" else "types in structure items" in
  add_set_with_tbl ~tbl ~id ~set ~descr

let add_set ~kind ~is_exn id ~set =
  let exn_poss =
    match is_exn with
    | `Yes -> [true]
    | `No -> [false]
    | `Both -> [true; false] in
  let add_poss =
    match kind with
    | `Str -> [add_str_set]
    | `Sig -> [add_sig_set]
    | `Both -> [add_str_set; add_sig_set] in
  List.iter (fun (add : ?is_exn:_ -> _) ->
    List.iter (fun is_exn ->
      add ~is_exn id ~set
    ) exn_poss
  ) add_poss

(* General purpose code generation module *)

module Gen = struct
  (* Map of record field source locations to their default expression *)
  let record_defaults : (Loc.t, Ast.expr) Hashtbl.t = Hashtbl.create 0

  let find_record_default loc =
    try Some (Hashtbl.find record_defaults loc) with Not_found -> None

  let gensym =
    let cnt = ref 0 in
    fun ?(prefix = "_x") () ->
      incr cnt;
      sprintf "%s__%03i_" prefix !cnt

  (* Like Ast.exSem_of_list but for application *)
  let exApp_of_list l =
    let rec aux = function
      | [] -> Ast.ExNil Loc.ghost
      | [x] -> x
      | x :: xs ->
        let loc = Ast.loc_of_expr x in
        <:expr@loc< $aux xs$ $x$ >>
    in
    aux (List.rev l)

  let rec tyArr_of_list = function
    | [] -> Ast.TyNil Loc.ghost
    | [x] -> x
    | x :: xs ->
      let loc = loc_of_ctyp x in
      <:ctyp@loc< $x$ -> $tyArr_of_list xs$ >>

  let rec paOr_of_list = function
    | [] -> Ast.PaNil Loc.ghost
    | [x] -> x
    | x :: xs ->
      let loc = loc_of_patt x in
      <:patt@loc< $x$ | $paOr_of_list xs$ >>

  module PP = Camlp4.Printers.OCaml.Make (Syntax)
  let conv_ctyp = (new PP.printer ())#ctyp

  let string_of_ctyp ctyp =
    try
      let buffer = Buffer.create 32 in
      Format.bprintf buffer "%a@?" conv_ctyp ctyp;
      Some (Buffer.contents buffer)
    with _ -> None

  let error tp ~fn ~msg =
    let loc = Ast.loc_of_ctyp tp in
    let failure =
      match string_of_ctyp tp with
      | Some tp_str -> sprintf "%s: %s\n%s" fn msg tp_str
      | None -> sprintf "%s: %s" fn msg
    in
    Loc.raise loc (Failure failure)

  let unknown_type tp fn = error tp ~fn ~msg:"unknown type"

  let rec ty_var_list_of_ctyp tp acc =
    match tp with
    | <:ctyp< $tp1$ $tp2$ >> ->
        ty_var_list_of_ctyp tp1 (ty_var_list_of_ctyp tp2 acc)
    | <:ctyp< '$param$ >> -> param :: acc
    | _ -> invalid_arg "ty_var_list_of_ctyp"

  let rec get_rev_id_path tp acc =
    match tp with
    | <:ident< $id1$ . $id2$ >> -> get_rev_id_path id2 (get_rev_id_path id1 acc)
    | <:ident< $lid:id$ >> | <:ident< $uid:id$ >> -> id :: acc
    | _ -> invalid_arg "get_rev_id_path"

  let mk_ident _loc str =
    let first = str.[0] in
    if first >= 'A' && first <= 'Z' then <:ident< $uid:str$ >>
    else <:ident< $lid:str$ >>

  let rec ident_of_rev_path _loc = function
    | [str] -> mk_ident _loc str
    | str :: strs ->
        <:ident< $ident_of_rev_path _loc strs$ . $mk_ident _loc str$ >>
    | _ -> invalid_arg "ident_of_rev_path"

  let rec get_appl_path _loc = function
    | <:ctyp< $id:id$ >> -> id
    | <:ctyp< $tp$ $_$ >> -> get_appl_path _loc tp
    | _ -> failwith "get_appl_path: unknown type"

  let abstract _loc = List.fold_right (fun p e -> <:expr< fun $p$ -> $e$ >>)
  let apply _loc = List.fold_left (fun f arg -> <:expr< $f$ $arg$ >>)

  let switch_tp_def ~alias ~sum ~record ~variants ~mani ~nil tp =
    let rec loop = function
      | <:ctyp< private $tp$ >> -> loop tp
      | <:ctyp@loc< [ $alts$ ] >> -> sum loc alts
      | <:ctyp@loc< [< $row_fields$ ] >> | <:ctyp@loc< [> $row_fields$ ] >>
      | <:ctyp@loc< [= $row_fields$ ] >> -> variants loc row_fields
      | <:ctyp@loc< $id:_$ >>
      | <:ctyp@loc< ( $tup:_$ ) >>
      | <:ctyp@loc< $_$ -> $_$ >>
      | <:ctyp@loc< '$_$ >>
      | <:ctyp@loc< $_$ $_$ >> as tp_def -> alias loc tp_def
      | <:ctyp@loc< { $flds$ } >> -> record loc flds
      | <:ctyp@loc< $tp1$ == $tp2$ >> -> mani loc tp1 tp2
      | <:ctyp@loc< >> -> nil loc
      | tp -> unknown_type tp "switch_tp_def"
    in
    loop tp

  let rec mk_expr_lst _loc = function
    | [] -> <:expr< [] >>
    | e :: es -> <:expr< [$e$ :: $mk_expr_lst _loc es$] >>

  let rec mk_patt_lst _loc = function
    | [] -> <:patt< [] >>
    | p :: ps -> <:patt< [$p$ :: $mk_patt_lst _loc ps$] >>

  let get_tparam_id = function
    | <:ctyp< '$id$ >> | <:ctyp< +'$id$ >> | <:ctyp< -'$id$ >> -> id
    | tp -> error tp ~fn:"get_tparam_id" ~msg:"not a type parameter"

  exception Stop
  let type_is_recursive short_circuit type_name = object (self)
    inherit fold as super
    method ctyp ctyp =
      match short_circuit ctyp with
      | Some false -> self
      | Some true -> raise Stop
      | None ->
        match ctyp with
        | <:ctyp< $_$ : $ctyp$ >> ->
          (* or else we would say that [type t = { t : int }] is recursive *)
          self#ctyp ctyp
        | <:ctyp< $lid:id$ >> -> if id = type_name then raise Stop else self
        | ctyp -> super#ctyp ctyp
  end
  let type_is_recursive ?(stop_on_functions = true) ?(short_circuit = fun _ -> None) type_name tp =
    let short_circuit =
      if stop_on_functions then
        function
        | <:ctyp< ( ~ $_$ : $_$ ) -> $_$ >>
        | <:ctyp< ( ? $_$ : $_$ ) -> $_$ >>
        | <:ctyp< $_$ -> $_$ >> -> Some false
        | ctyp -> short_circuit ctyp
      else short_circuit
    in
    try ignore ((type_is_recursive short_circuit type_name)#ctyp tp); false
    with Stop -> true

  let drop_variance_annotations =
    (map_ctyp (function
      | <:ctyp@loc< +'$var$ >> | <:ctyp@loc< -'$var$ >> -> <:ctyp@loc< '$var$ >>
      | tp -> tp))#ctyp

  let vars_of = object (self)
    inherit fold as super
    val vars = []
    method vars = vars
    method! ctyp _ = self
    method! ident = function
    | <:ident< $lid:v$ >> -> {< vars = v :: vars >}
    | ident -> super#ident ident
    method! patt = function
    | <:patt< $_$ = $p$ >> -> self#patt p
    | p -> super#patt p
  end
  let lids_of_patt patt =
    (vars_of#patt patt)#vars

  let ignore_everything = object (self)
    inherit map as super
    method sig_item sig_item =
      match super#sig_item sig_item with
      | <:sig_item@loc< value $id$ : $ctyp$ >> ->
        <:sig_item@loc< value $id$ : _no_unused_value_warning_ $ctyp$ >>
      | sig_item -> sig_item
    method str_item str_item =
      match super#str_item str_item with
      | <:str_item@loc< value $rec:_$ $bindings$ >> as str_item -> (
        match self#ignore_binding bindings with
        | None ->
          str_item
        | Some more_bindings ->
          <:str_item@loc<
            $str_item$;
            value $more_bindings$;
          >>
      )
      | str_item -> str_item
    method ignore_binding = function
      | Ast.BiAnt _
      | <:binding< >> -> None
      | <:binding@loc< $b1$ and $b2$ >> -> (
        match self#ignore_binding b1, self#ignore_binding b2 with
        | b, None
        | None, b -> b
        | Some b1, Some b2 ->
          Some <:binding@loc< $b1$ and $b2$ >>
        )
      | <:binding@loc< $patt$ = $_$ >> ->
        match lids_of_patt patt with
        | [] -> None
        | h :: t ->
          let mk_binding acc lid = <:binding@loc< $acc$ and _ = $lid:lid$ >> in
          Some (List.fold_left mk_binding <:binding@loc< _ = $lid:h$ >> t)
  end

   let delay_sig_item sig_item =
     Signature_stack.Item.delay_sig (Signature_stack.top ()) sig_item
end

(* Functions for interpreting derivation types *)

let find_generator ~name haystack = (); fun rec_ entry (needle,arg,gen_to_remove) ->
  let seen = Hashtbl.create 0 in
  let generators = ref [] in
  (* enumerating the generators reachable from [needle] in no particular
     order. If some generators depend on code generated by other generators,
     we should probably change that and have a predictable order.
     Set diff A \ B is implemented by marking all elements of B as seen
     without adding them to [generators] and then visiting A. *)
  let rec aux ~add = function
    | [] -> ()
    | needle :: rest ->
      if Hashtbl.mem seen needle then aux ~add rest
      else (
        Hashtbl.add seen needle ();
        match Hashtbl.find haystack needle with
        | `Set set -> aux ~add (set @ rest)
        | `Actual_generator g ->
          if add then generators := g :: !generators;
          aux ~add rest
      ) in
  let aux_with_error ~add needle =
    try aux ~add [needle]
    with Not_found ->
       (* the first lookup is the only one that can fail because we check
          when we define sets that they only reference known generators *)
      let keys = Hashtbl.fold (fun key _ acc -> key :: acc) haystack [] in
      let gen_names = String.concat ", " keys in
      let msg =
        Printf.sprintf
          "Pa_type_conv: \
          %S is not a supported %s generator. (supported generators: %s)"
          needle
          name
          gen_names in
      failwith msg in
  List.iter (aux_with_error ~add:false) gen_to_remove;
  aux_with_error ~add:true needle;

  List.rev_map (fun genf ->
    genf arg rec_ entry
  ) !generators

let generate = find_generator ~name:"type" generators

let gen_derived_defs _loc rec_ tp drvs =
  let coll drv der_sis = <:str_item< $der_sis$; $stSem_of_list (generate rec_ tp drv)$ >> in
  List.fold_right coll drvs <:str_item< >>

let generate_exn = find_generator ~name:"exception" exn_generators

let gen_derived_exn_defs _loc tp drvs =
  let coll drv der_sis = <:str_item< $der_sis$; $stSem_of_list (generate_exn false tp drv)$ >> in
  List.fold_right coll drvs <:str_item< >>

let sig_generate = find_generator ~name:"signature" sig_generators

let gen_derived_sigs _loc rec_ tp drvs =
  let coll drv der_sis = <:sig_item< $der_sis$; $sgSem_of_list (sig_generate rec_ tp drv)$ >> in
  List.fold_right coll drvs (SgNil _loc)

let sig_exn_generate =
  find_generator ~name:"signature exception" sig_exn_generators

let gen_derived_exn_sigs _loc tp drvs =
  let coll drv der_sis = <:sig_item< $der_sis$; $sgSem_of_list (sig_exn_generate false tp drv)$ >> in
  List.fold_right coll drvs (SgNil _loc)

let remember_record_field_generators el drvs =
  let act drv =
    let gen = find_generator ~name:"record field" record_field_generators in
    ignore (gen false el drv : unit list)
  in
  List.iter act drvs

(* rewriting of non recursive type definition
   [type nonrec t = t]
   is rewritten
   [include (struct
      type fresh = t
      type t = fresh
    end : sig
      type fresh = t
      type t = fresh
    end with type fresh := t
   )]
   This way, none of the intermediate types are exposed.
*)

(*  Note that type definitions like

       type nonrec t = t = {foo:int}

   won't work. You might think that it could be rewritten as:

       include (struct
         type fresh = t = {foo:int}
         type t = fresh = {foo:int}
       end : sig
         type fresh = t = {foo:int}
         type t = fresh = {foo:int}
       end with type fresh := t)

    but the compiler complains on fresh := t, and fresh := t = {foo:int} is not valid
    syntax.
*)

module Rewrite_tds : sig
  val sig_ : Ast.loc -> bool -> Ast.ctyp -> Ast.sig_item
  val str_ : Ast.loc -> bool -> Ast.ctyp -> Ast.str_item
end = struct
  module StringSet = Set.Make(String)
  module StringMap = Map.Make(String)

  let bound_names = object
    inherit fold as super
    val bound_names = []
    method bound_names = bound_names
    method! ctyp = function
      | Ast.TyDcl (_loc, n, _tpl, _tk, _cl) ->
        {< bound_names = n :: bound_names >}
      | ctyp ->
        super#ctyp ctyp
  end

  let bound_names td =
    (bound_names#ctyp td)#bound_names

  let rec match_type_constructor acc = function
    | <:ctyp@_loc< $t1$ $t2$ >> ->
      match_type_constructor ((t2,_loc) :: acc) t1
    | <:ctyp@_loc< $lid:id$ >> ->
      Some (id, _loc, acc)
    | _ ->
      None
  let rebuild_type_constructor (id, _loc, params) =
    List.fold_left (fun acc (param, _loc) ->
      <:ctyp< $acc$ $param$ >>
    ) <:ctyp< $lid:id$ >> params

  let referenced_names used_bound bound = object (self)
    inherit map as super
    method! ctyp t =
      match t with
      | <:ctyp@loc< $lhs$ : $rhs$ >> ->
        <:ctyp@loc< $lhs$ : $self#ctyp rhs$ >>
      | _ ->
        match match_type_constructor [] t with
        | Some (id, _loc, params) ->
          let id =
            try
              let new_ = StringMap.find id bound in
              used_bound := StringMap.add id (_loc, List.length params) !used_bound;
              new_
            with Not_found -> id in
          let params = List.map (fun (param, _loc) -> (self#ctyp param, _loc)) params in
          rebuild_type_constructor (id, _loc, params)
        | None ->
          super#ctyp t
  end

  let gen =
    let r = ref (-1) in
    fun () -> incr r; Printf.sprintf "__pa_nonrec_%d" !r

  let referenced_names td =
    let bound_names = bound_names td in
    let bound_names_map =
      List.fold_left (fun acc name -> StringMap.add name (gen ()) acc)
        StringMap.empty bound_names in
    let used_bound = ref StringMap.empty in
    let td = (referenced_names used_bound bound_names_map)#ctyp td in
    let bound_names_map =
      StringMap.fold (fun key v acc ->
        try
          let arity = StringMap.find key !used_bound in
          StringMap.add key (v, arity) acc
        with Not_found -> acc
      ) bound_names_map StringMap.empty in
    td, bound_names_map, used_bound

  let params_of_arity (_loc, arity) =
    Array.to_list (
      Array.init arity (fun i ->
        <:ctyp< '$lid:sprintf "a%d" i$ >>
      )
    )
  let constructor_of_arity t (_loc, arity) =
    let args = List.map (fun param -> (param, _loc)) (params_of_arity (_loc, arity)) in
    rebuild_type_constructor (t, _loc, args)

  let build_common _loc td =
    let td2, map, _set = referenced_names td in
    StringMap.fold (fun k (v, arity) acc ->
      let tydcl =
        TyDcl (_loc, v, params_of_arity arity, constructor_of_arity k arity, [])
      in
      let new_constraints =
        <:with_constr< type $constructor_of_arity v arity$ := $constructor_of_arity k arity$ >>
      in
      match acc with
      | None ->
        Some (tydcl, td2, new_constraints)
      | Some (td1, td2, constraints) ->
        let td1 = <:ctyp< $td1$ and $tydcl$ >> in
        let constraints = <:with_constr< $constraints$ and $new_constraints$ >> in
        Some (td1, td2, constraints))
    map None

  let str_ _loc rec_ td =
    if rec_ then <:str_item< type $td$ >> else
    match build_common _loc td with
    | None -> <:str_item< type $td$ >>
    | Some (td1, td2, constraints) ->
      <:str_item< include (struct type $td1$; type $td2$; end : sig
        type $td1$; type $td2$;
      end with $constraints$) >>

  let sig_ _loc rec_ td =
    if rec_ then <:sig_item< type $td$ >> else
    match build_common _loc td with
    | None -> <:sig_item< type $td$ >>
    | Some (td1, td2, constraints) ->
      <:sig_item< include (sig type $td1$; type $td2$;
      end with $constraints$) >>
end

(* Syntax extension *)

open Syntax

let is_prefix ~prefix x =
  let prefix_len = String.length prefix in
  String.length x >= prefix_len && prefix = String.sub x 0 prefix_len

let chop_prefix ~prefix x =
  if is_prefix ~prefix x then
    let prefix_len = String.length prefix in
    Some (String.sub x prefix_len (String.length x - prefix_len))
  else None

let get_default_path _loc =
  try
    let prefix = Sys.getenv "TYPE_CONV_ROOT" in
    match chop_prefix ~prefix (Loc.file_name (Loc.make_absolute _loc)) with
    | Some x -> x ^ "#"
    | None -> Loc.file_name _loc
  with _ -> Loc.file_name _loc

let set_conv_path_if_not_set _loc =
  if !conv_path_ref = Not_initialized || !Sys.interactive then
    let conv_path = get_default_path _loc in
    conv_path_ref := Path (conv_path, [conv_path])

let found_module_name =
  Gram.Entry.of_parser "found_module_name" (fun strm ->
    match Stream.npeek 1 strm with
    | [(UIDENT name, token_info)] ->
        set_conv_path_if_not_set (Gram.token_location token_info);
        push_conv_path name;
        Stream.junk strm;
        name
    | _ -> raise Stream.Failure)

let rec fetch_generator_arg paren_count acc strm =
  let token, token_info as elt = Stream.next strm in
  match token with
  | KEYWORD "(" ->
    fetch_generator_arg (paren_count + 1) (elt :: acc) strm
  | KEYWORD ")" when paren_count = 1 ->
    (EOI, token_info) :: acc
  | KEYWORD ")" ->
    fetch_generator_arg (paren_count - 1) (elt :: acc) strm
  | EOI ->
    Loc.raise (Gram.token_location token_info) (Stream.Error "')' missing")
  | _ ->
    fetch_generator_arg paren_count (elt :: acc) strm

let rec_ =
  Gram.Entry.of_parser "nonrec" (fun strm ->
    match Stream.peek strm with
    | Some (LIDENT "nonrec", _) ->
      Stream.junk strm;
      false
    | _ ->
      true)

let generator_arg =
  Gram.Entry.of_parser "generator_arg" (fun strm ->
    match Stream.peek strm with
    | Some (KEYWORD "(", _) ->
        Stream.junk strm;
        Some (List.rev (fetch_generator_arg 1 [] strm))
    | _ -> None)

let mk_ctyp _loc name params =
  List.fold_left (fun acc x ->
    Ast.TyApp (_loc, acc, Gen.drop_variance_annotations x)
  ) <:ctyp< $lid:name$ >> params

let rec types_used_by_type_conv = function
  | Ast.TyDcl (_loc, name, tps, _rhs, _cl) ->
    <:str_item< value _ (_ : $mk_ctyp _loc name tps$) = () >>
  | Ast.TyAnd (_loc, td1, td2) ->
    <:str_item<
      $types_used_by_type_conv td1$;
      $types_used_by_type_conv td2$
    >>
  | _ -> assert false

let quotation_str_item = Gram.Entry.mk "quotation_str_item";;

DELETE_RULE Gram str_item: "module"; a_UIDENT; module_binding0 END;
DELETE_RULE Gram str_item: "type"; type_declaration END;
DELETE_RULE Gram sig_item: "type"; type_declaration END;
DELETE_RULE Gram module_type: "sig"; sig_items; "end" END;

EXTEND Gram
  GLOBAL: quotation_str_item str_item sig_item label_declaration module_type;

  str_item:
    [[
      "TYPE_CONV_PATH"; conv_path = STRING ->
        set_conv_path conv_path;
        <:str_item< >>
    ]];

  generator: [[
    (* disallowing arguments when subtracting because the meaning of things like
       [type t with typehash(something) - typehash(somethingelse)] is unclear *)
    [ id = LIDENT; l = LIST1 [ "-"; x = LIDENT -> x ] -> (id, None, l)
    | id = LIDENT; arg = generator_arg -> (id, arg, []) ]
  ]];

  quotation_str_item: [[
    [ "type"; rec_ = rec_; tds = type_declaration; "with"; drvs = LIST1 generator SEP "," ->
        let str_item = gen_derived_defs _loc rec_ tds drvs in
        Gen.ignore_everything#str_item str_item
  ]]];

  str_item:
    [[
    [ "type"; rec_ = rec_; tds = type_declaration; "with"; drvs = LIST1 generator SEP "," ->
        set_conv_path_if_not_set _loc;
        let str_item = gen_derived_defs _loc rec_ tds drvs in
        let str_item = Gen.ignore_everything#str_item str_item in
        <:str_item<
          $Rewrite_tds.str_ _loc rec_ tds$;
          $types_used_by_type_conv tds$;
          $str_item$
        >>
    | "type"; rec_ = rec_; tds = type_declaration ->
        Rewrite_tds.str_ _loc rec_ tds
    ]]];

  str_item:
    [[
      "exception"; tds = constructor_declaration; "with";
      drvs = LIST1 generator SEP "," ->
        set_conv_path_if_not_set _loc;
        let str_item = gen_derived_exn_defs _loc tds drvs in
        let str_item = Gen.ignore_everything#str_item str_item in
        <:str_item< exception $tds$; $str_item$ >>
    ]];

  str_item:
    [[
      "module"; i = found_module_name; mb = module_binding0 ->
        pop_conv_path ();
        <:str_item< module $i$ = $mb$ >>
    ]];

  start_of_sig:
    [[ "sig" -> Signature_stack.push () ]];

  module_type:
    [[
      start_of_sig; sg = sig_items; "end" ->
      match Signature_stack.Item.delayed_sigs (Signature_stack.pop ()) with
      | [] -> <:module_type< sig $sg$ end >>
      | delayed_sigs ->
        let delayed_sigs = List.map Gen.ignore_everything#sig_item delayed_sigs in
        <:module_type< sig $sg$; $list:delayed_sigs$ end >>
    ]];

  sig_item:
    [[
    [ "type"; rec_ = rec_; tds = type_declaration; "with"; drvs = LIST1 generator SEP "," ->
        set_conv_path_if_not_set _loc;
        let sig_item = gen_derived_sigs _loc rec_ tds drvs in
        let sig_item = Gen.ignore_everything#sig_item sig_item in
        <:sig_item< $Rewrite_tds.sig_ _loc rec_ tds$; $sig_item$ >>
    | "type"; rec_ = rec_; tds = type_declaration ->
        Rewrite_tds.sig_ _loc rec_ tds
    ]]];

  sig_item:
    [[
      "exception"; cd = constructor_declaration; "with";
      drvs = LIST1 generator SEP "," ->
        set_conv_path_if_not_set _loc;
        let sig_item = gen_derived_exn_sigs _loc cd drvs in
        let sig_item = Gen.ignore_everything#sig_item sig_item in
        <:sig_item< exception $cd$; $sig_item$ >>
    ]];

  label_declaration:
    [[ name = a_LIDENT; ":"; tp = poly_type;
       "with"; drvs = LIST1 generator SEP "," ->
       let label_tp = Ast.TyLab (_loc, name, tp) in
       remember_record_field_generators label_tp drvs;
       <:ctyp< $lid:name$ : $tp$ >>
    | "mutable"; name = a_LIDENT; ":"; tp = poly_type;
      "with"; drvs = LIST1 generator SEP "," ->
       let label_tp = Ast.TyMut (_loc, Ast.TyLab (_loc, name, tp)) in
        remember_record_field_generators label_tp drvs;
        <:ctyp< $lid:name$ : mutable $tp$ >>
    ]];
END

let type_conv_quotation loc _loc_name_opt cnt_str =
  set_conv_path_if_not_set loc;
  let str_item = Gram.parse_string quotation_str_item loc cnt_str in
  <:module_expr@loc< struct $str_item$ end >>

let () =
  (* <:type_conv< type t = ... >> outputs the generated code but discards the type definition *)
  Quotation.add "type_conv" Quotation.DynAst.module_expr_tag type_conv_quotation

(* Record field defaults *)

(* Add "default" to set of record field generators *)
let () =
  add_record_field_generator_with_arg "default" Syntax.expr
    (fun expr_opt tp ->
      let loc = Ast.loc_of_ctyp tp in
      let default =
        match expr_opt with
        | Some expr -> expr
        | None -> Loc.raise loc (Failure "could not parse default expression")
      in
      if Hashtbl.mem Gen.record_defaults loc then
        Loc.raise loc (Failure "several default expressions are given");
      Hashtbl.replace Gen.record_defaults loc default)

(* Removal of warnings (in signatures).
   Because ocaml gives warnings but doesn't give a way to deactivate them, we have
   plenty in the generated code. The most annoyings ones are the ones in signatures,
   because they are harder to remove.
   For instance:
     module M : sig type t = [ `A ] with sexp end = ...
   is likely to generate a warning 'unused value t_of_sexp__' in the signature (the same
   warning in an implementation would be already removed).
   To work around that, for every 'val name : type' auto generated, we insert a
   'val name : _no_unused_value_warning_' next to it.
   And in a second step (could probably be done in one step, but it would be complicated),
   we try to generate an expression that will use these name (which we recognize thanks to
   the '_no_unused_value_warning_' mark).
   To use a 'val name : type' in a context like:
     module M : sig val name : type end = ...
   you simply need to do:
     let _ = M.name
   And there are other tricks depending on where the signature item appear. The removal of
   warning doesn't handle all possible ways of generating warnings. *)

module String_set = Set.Make(String)

let qualify_idents loc m idents =
  List.map (fun i -> <:ident@loc< $uid:m$.$i$ >>) idents
let use_idents loc idents =
  List.fold_left
    (fun acc i -> <:str_item@loc< $acc$; value _ = $id:i$; >>)
    <:str_item@loc< >> idents
let use_idents_in loc idents body =
  List.fold_left
    (fun acc i -> <:expr@loc< let _ = $id:i$ in $acc$ >>)
    body idents

let ignore = object (self)
  inherit Ast.map as super

  method expr = function
    | <:expr@loc< let module $uid:m$ : $module_type$ = $module_expr$ in $body$ >> ->
      let module_expr = self#module_expr module_expr in
      let idents, module_type = self#ignore_module_type module_type in
      let body = self#expr body in
      let body = use_idents_in loc (qualify_idents loc m idents) body in
      <:expr@loc< let module $uid:m$ : $module_type$ = $module_expr$ in $body$ >>
    | expr -> super#expr expr

  method str_item = function
    | <:str_item@loc< module type $s$ = $module_type$ >> ->
      let idents, module_type = self#ignore_module_type module_type in
      let warnings_removal = use_idents loc (qualify_idents loc s idents) in
      if idents = []
      then <:str_item@loc< module type $s$ = $module_type$; >>
      else <:str_item@loc<
        module type $s$ = $module_type$;
        value () = if True then () else begin
          let module M($s$ : $uid:s$) = struct
            $warnings_removal$;
          end in
          ()
        end;
      >>

    | <:str_item@loc< module $uid:m$ : $module_type$ = $module_expr$ >> ->
      let module_expr = self#module_expr module_expr in
      let idents, module_type = self#ignore_module_type module_type in
      let warnings_removal = use_idents loc (qualify_idents loc m idents) in
      <:str_item@loc< module $uid:m$ : $module_type$ = $module_expr$; $warnings_removal$ >>

    | StMod _
    | StSem _
    | StInc _
    | StNil _
    | StCls _
    | StClt _
    | StDir _
    | StExc _
    | StExp _
    | StExt _
    | StRecMod _
    | StOpn _
    | StTyp _
    | StVal _
    | StAnt _ as str_item ->
      super#str_item str_item

  method fold_map_on_functor_arg warnings_removal = function
    | MeFun (loc, s, mt, me) ->
      (* wouldn't be quite right if you have a functor that takes several arguments with
         the same name, but who would do that anyway? *)
      let idents, mt = self#ignore_module_type mt in
      let more_warnings_removal = use_idents loc (qualify_idents loc s idents) in
      let warnings_removal = <:str_item@loc< $warnings_removal$; $more_warnings_removal$ >> in
      let me = self#fold_map_on_functor_arg warnings_removal me in
      MeFun (loc, s, mt, me)
    | me ->
      match self#module_expr me with
      | MeStr (loc, str_item) ->
        MeStr (loc, <:str_item@loc< $warnings_removal$; $str_item$ >>)
      | MeTyc (loc, MeStr (loc2, str_item), mt) ->
        MeTyc (loc, MeStr (loc2, <:str_item@loc2< $warnings_removal$; $str_item$ >>), mt)
      | me ->
        (* not ignoring the warnings in this case because we don't even know if $me$ is
           a functor or not, which makes it impossible to find a way of inserting
           $warnings_removal$ *)
        me

  method module_expr = function
    | MeFun (loc, _, _, _) as me -> self#fold_map_on_functor_arg <:str_item@loc< >> me

    | MeStr _
    | MeTyc _
    | MeNil _
    | MeId _
    | MePkg _
    | MeAnt _
    | MeApp _ as me ->
      super#module_expr me

  (* Strip all the 'markers' that have not been handled *)
  method sig_item = function
    | <:sig_item@loc< value $id$ : _no_unused_value_warning_ $ctyp$  >> ->
      <:sig_item@loc< value $id$ : $ctyp$ >>
    | sig_item -> super#sig_item sig_item

  method ignore_module_type = function
    | MtNil _
    | MtId _
    | MtFun _
    | MtQuo _
    | MtOf _
    | MtAnt _ as mt ->
      [], self#module_type mt
    | MtSig (loc, sig_item) ->
      let idents, sig_item = self#ignore_sig_item sig_item in
      idents, MtSig (loc, sig_item)
    | MtWit (loc, module_type, with_constr) ->
      let idents, module_type = self#ignore_module_type module_type in
      idents, MtWit (loc, module_type, with_constr)

  method ignore_sig_item sig_item =
    let next_defs = String_set.empty in
    let _next_defs, acc, sig_item = self#ignore_sig_item_aux next_defs [] sig_item in
    acc, sig_item
  method ignore_sig_item_aux next_defs acc = function
     | <:sig_item@loc< value $id$ : _no_unused_value_warning_ $ctyp$ >> ->
      if String_set.mem id next_defs then
        next_defs, acc, <:sig_item@loc< >>
      else
        let next_defs = String_set.add id next_defs in
        let sig_item = <:sig_item@loc< value $id$ : $self#ctyp ctyp$ >> in
        next_defs, <:ident@loc< $lid:id$ >> :: acc, sig_item
     | <:sig_item< value $id$ : $_$ >> as sig_item ->
       let sig_item = self#sig_item sig_item in
       let next_defs = String_set.add id next_defs in
       next_defs, acc, sig_item
    | <:sig_item@loc< module $uid:m$ : $module_type$ >> ->
      let new_idents, module_type = self#ignore_module_type module_type in
      next_defs, acc @ qualify_idents loc m new_idents, <:sig_item@loc< module $uid:m$ : $module_type$ >>
    | <:sig_item@loc< $si1$; $si2$ >> ->
      (* in here, we are traversing from right to left, so that when see an identifier
         we already know whether a further 'val ...' will hide it *)
      let next_defs, acc, si2 = self#ignore_sig_item_aux next_defs acc si2 in
      let next_defs, acc, si1 = self#ignore_sig_item_aux next_defs acc si1 in
      next_defs, acc, <:sig_item@loc< $si1$; $si2$ >>
    | sig_item -> next_defs, acc, self#sig_item sig_item
end

let strip = object
  inherit Ast.map as super

  method sig_item = function
  | <:sig_item@loc< value $id$ : _no_unused_value_warning_ $ctyp$  >> ->
    <:sig_item@loc< value $id$ : $ctyp$ >>
  | sig_item -> super#sig_item sig_item
end

let () =
  (* above, the parser used 'sig' and 'end' as anchors but an mli is a signature
     without the sig and end. So here we catch all the elements that have been inserted
     at toplevel in the mli *)
  let _, current_sig_parser = Register.current_parser () in
  Register.register_sig_item_parser (fun ?directive_handler _loc stream ->
    let mli = current_sig_parser ?directive_handler _loc stream in
    match Signature_stack.Item.delayed_sigs Signature_stack.bottom with
    | [] -> mli
    | sig_items -> <:sig_item< $list: mli :: sig_items$ >>
  );
  AstFilters.register_sig_item_filter strip#sig_item;
  AstFilters.register_str_item_filter ignore#str_item;
  AstFilters.register_topphrase_filter ignore#str_item
