open Zi_lib
open Ir

let i32_0 = Int32.of_int 0
let i32_1 = Int32.of_int 1
let i32_of_bool x = if x then i32_1 else i32_0

module Make () = struct
  let logf fmt = Logger.make_logf __MODULE__ fmt

  module Environment = struct
    type env =
      | Env of { procmap : procid Ast.IdMap.t; varmap : reg Ast.IdMap.t }

    let empty =
      let procmap = Ast.IdMap.empty in
      let varmap = Ast.IdMap.empty in
      Env { procmap; varmap }

    let add_proc id procid (Env { procmap; varmap }) =
      logf "Environment.add_proc: added mapping %s -> %s"
        (Ast.string_of_identifier id)
        (Ir_utils.string_of_procid procid);
      let procmap = Ast.IdMap.add id procid procmap in
      Env { procmap; varmap }

    let add_var id reg (Env { procmap; varmap }) =
      logf "Environment.add_var: added mapping %s -> %s"
        (Ast.string_of_identifier id)
        (Ir_utils.string_of_reg reg);
      let varmap = Ast.IdMap.add id reg varmap in
      Env { procmap; varmap }

    let lookup_proc id (Env { procmap; _ }) =
      try Ast.IdMap.find id procmap
      with Not_found ->
        failwith
        @@ Format.sprintf "Unknown procedure identifier: %s"
             (Ast.string_of_identifier id)

    let lookup_var id (Env { varmap; _ }) =
      try Ast.IdMap.find id varmap
      with Not_found ->
        failwith
        @@ Format.sprintf "Unknown variable identifier: %s"
             (Ast.string_of_identifier id)

    let symbols (Env { procmap; _ }) = List.map snd (Ast.IdMap.bindings procmap)
  end

  (* --------------------------------------------------- *)

  module type SContext = sig
    val cfg : ControlFlowGraph.t
    val node2type : (Ast.node_tag, Types.normal_type) Hashtbl.t
    val allocate_register : unit -> reg
  end

  (* --------------------------------------------------- *)
  module Translator (M : SContext) = struct
    open M

    let append_instruction l i =
      let block = ControlFlowGraph.block cfg l in
      logf "append_instruction: block=%s: %s"
        (Ir_utils.string_of_label l)
        (Ir_utils.string_of_instr i);
      ControlFlowGraph.set_block cfg l (block @ [ i ])

    let set_terminator l t =
      logf "set_terminator: block=%s: %s"
        (Ir_utils.string_of_label l)
        (Ir_utils.string_of_terminator t);
      ControlFlowGraph.set_terminator cfg l t

    let set_jump l_from l_to =
      set_terminator l_from @@ T_Jump l_to;
      ControlFlowGraph.connect cfg l_from l_to

    let set_return l_from xs =
      set_terminator l_from @@ T_Return xs;
      ControlFlowGraph.connect cfg l_from (ControlFlowGraph.exit_label cfg)

    let set_branch cond a b l_from l_to1 l_to2 =
      set_terminator l_from @@ T_Branch (cond, a, b, l_to1, l_to2);
      ControlFlowGraph.connect cfg l_from l_to1;
      ControlFlowGraph.connect cfg l_from l_to2

    let allocate_block () =
      let l = ControlFlowGraph.allocate_block cfg in
      logf "allocate_block: new block is %s" (Ir_utils.string_of_label l);
      l

    (* --------------------------------------------------- *)
    let rec translate_expression env current_bb e =
      logf "translate_expression: >>> %s: %s -- current_bb=%s"
        (Ast.string_of_location e.Ast.loc)
        (Ast_printer.show_expression e)
        (Ir_utils.string_of_label current_bb);
      let current_bb, res = _translate_expression env current_bb e in
      logf "translate_expression: <<< %s: %s - current_bb=%s res=%s"
        (Ast.string_of_location e.Ast.loc)
        (Ast_printer.show_expression e)
        (Ir_utils.string_of_label current_bb)
        (Ir_utils.string_of_expr res);
      (current_bb, res)

    and _translate_expression env current_bb e =
      match e.data with
      | Ast.EXPR_Int value -> (current_bb, E_Int value)
      | Ast.EXPR_Call { callee; arguments } ->
          (* Odróżniamy zwijanie typu rekurencyjnego od wywałania zwykłej funkcji
           * patrząc czy nazwa funkcji jest taka sama jak nazwa typu wyrażenia.
           *)
          failwith "Not yet implemented"
      | _ -> failwith "Not yet implemented"

    (* --------------------------------------------------- *)
    and translate_condition env current_bb alt_bb e =
      logf "translate_condition: >>> %s: %s -- current_bb=%s alt_bb=%s"
        (Ast.string_of_location e.Ast.loc)
        (Ast_printer.show_expression e)
        (Ir_utils.string_of_label current_bb)
        (Ir_utils.string_of_label alt_bb);
      let current_bb = _translate_condition env current_bb alt_bb e in
      logf "translate_condition: <<< %s: %s - current_bb=%s"
        (Ast.string_of_location e.Ast.loc)
        (Ast_printer.show_expression e)
        (Ir_utils.string_of_label current_bb);
      current_bb

    and _translate_condition env current_bb alt_bb e =
      match e.data with
      | Ast.EXPR_Bool true -> current_bb
      | Ast.EXPR_Bool false ->
          set_jump current_bb alt_bb;
          allocate_block ()
      | _ -> failwith "Not yet implemented"

    (* --------------------------------------------------- *)

    let rec translate_statement env current_bb s =
      logf "translate_statement: >>> %s: %s -- current_bb=%s"
        (Ast.string_of_location s.Ast.loc)
        (String.concat ";" @@ Ast_printer.showxs_statement s)
        (Ir_utils.string_of_label current_bb);
      let env, current_bb = _translate_statement env current_bb s in
      logf "translate_statement: <<< %s: %s -- current_bb=%s"
        (Ast.string_of_location s.Ast.loc)
        (String.concat ";" @@ Ast_printer.showxs_statement s)
        (Ir_utils.string_of_label current_bb);
      (env, current_bb)

    and _translate_statement env current_bb stmt =
      match stmt.Ast.data with _ -> failwith "Not yet implemented"

    let bind_var_declaration env vardecl =
      let r = allocate_register () in
      let env =
        Environment.add_var (Ast.identifier_of_var_declaration vardecl) r env
      in
      (env, r)

    let bind_formal_parameters env xs =
      let f env x = fst (bind_var_declaration env x) in
      List.fold_left f env xs

    let translate_global_definition env def =
      match def.Ast.data with
      | Ast.GDEF_Function { id; body; formal_parameters; _ } ->
          logf "translate_global_definition: =========== %s - %s"
            (Ast.string_of_location def.loc)
            (Ast.string_of_identifier id);
          let procid = Environment.lookup_proc id env in
          let frame_size = 0 in
          let env = bind_formal_parameters env formal_parameters in
          let formal_parameters = List.length formal_parameters in
          let proc =
            Procedure
              { procid; cfg; frame_size; allocate_register; formal_parameters }
          in
          let first_bb = allocate_block () in
          let _, last_bb = translate_statement env first_bb body in
          ControlFlowGraph.connect cfg last_bb (ControlFlowGraph.exit_label cfg);
          ControlFlowGraph.connect cfg
            (ControlFlowGraph.entry_label cfg)
            first_bb;
          [ proc ]
      | _ -> []
  end

  let make_allocate_register () =
    let counter = ref 0 in
    fun () ->
      let i = !counter in
      incr counter;
      REG_Tmp i

  let translate_global_definition node2type env gdef =
    let cfg = ControlFlowGraph.create () in
    let module T = Translator (struct
      let cfg = cfg
      let node2type = node2type
      let allocate_register = make_allocate_register ()
    end) in
    T.translate_global_definition env gdef

  let translate_module node2type env
      (Ast.ModuleDefinition { global_definitions; _ }) =
    List.flatten
    @@ List.map (translate_global_definition node2type env) global_definitions

  (* Wykorzystujemy środowisko wygenerowane przez typechecker
   * do dodania wszystkich nazw procedur (zarówno z pliku źródłowego
   * jak i z plików nagłówkowych) 
   *)
  let translate_module mdef node2type id_map =
    let open Types in
    let env =
      Ast.IdMap.fold
        (fun id st acc ->
          match st with
          | ST_Function _ ->
              let name = Format.sprintf "_%s" (Ast.string_of_identifier id) in
              Environment.add_proc id (Procid name) acc
          | _ -> acc)
        id_map Environment.empty
    in
    let procedures = translate_module node2type env mdef in
    Program { procedures; symbols = Environment.symbols env }
end
