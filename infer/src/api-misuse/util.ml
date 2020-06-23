module CFG = ProcCfg.Normal

let print_succ_nodes cfg = 
    let node = CFG.Node.underlying_node cfg in
    let preds_list = Procdesc.Node.get_succs node in
    Printf.printf "%s\n" (Location.to_string (CFG.Node.loc cfg));
    Stdlib.List.iteri (fun i n ->
      Printf.printf "%d %s\n" i (Location.to_string (CFG.Node.of_underlying_node n |> CFG.Node.loc))
    ) preds_list


let which_type_pp (e: Exp.t) =
  match e with
  | Var _ -> Printf.printf "VAR!\n"
  | UnOp _ -> Printf.printf "UNARY OP!\n"
  | BinOp _ -> Printf.printf "BINOP !\n"
  | Exn _ -> Printf.printf "EXNOP !\n"
  | Closure _ -> Printf.printf "CLOSURE !\n"
  | Const _ -> Printf.printf "CONST !\n"
  | Cast _ -> Printf.printf "CAST !\n"
  | Lvar _ -> Printf.printf "LAVR !\n"
  | Lfield _ -> Printf.printf "Lfield !\n"
  | Lindex _ -> Printf.printf "Lindex !\n"
  | Sizeof _ -> Printf.printf "Sizeof !\n"

let get_procname (cf : Procname.t) =
  match cf with
  | Procname.ObjC_Cpp cpp -> Some cpp
  | _ -> None

let get_Cfun (c : Const.t) =
  match c with
  | Cfun cf -> Some cf
  | _ -> None

let get_func_info (e : Exp.t) =
  match e with
  | Const c ->
      (match get_Cfun c with
      | Some cf -> get_procname cf
      | None -> None)
  | _ -> None

let is_assign_operator (e : Exp.t) =
  match get_func_info e with
  | Some {class_name; kind; method_name; parameters; template_args} ->
      if Stdlib.(=) method_name "operator=" then true else false
  | None -> false 
