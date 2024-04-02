(* Student 1: Ayan Mahmood
   Student 2: Romeo Lopez
   Pledge: I pledge my honor that I have abided by the Stevens Honor System *)

(* This file defines expressed values and environments *)

(* expressed values and environments are defined mutually recursively *)
type 'a tree = Empty | Node of 'a * 'a tree * 'a tree

type exp_val =
  | NumVal of int
  | BoolVal of bool
  | PairVal of exp_val*exp_val
  | TupleVal of exp_val list
  | TreeVal of exp_val tree
  | RecordVal of (string*exp_val) list
  | ProjVal of exp_val*string
type env =
  | EmptyEnv
  | ExtendEnv of string*exp_val*env


(* Environment Abstracted Result *)

type 'a result = Ok of 'a | Error of string

type 'a ea_result = env -> 'a result
  
let return : 'a -> 'a ea_result =
  fun v ->
  fun _env ->
  Ok v

let error : string -> 'a ea_result = fun s ->
  fun _env -> 
  Error s

let (>>=) : 'a ea_result -> ('a -> 'b ea_result) -> 'b ea_result = fun c f ->
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok v -> f v env

let (>>+) : env ea_result -> 'a ea_result -> 'a ea_result =
  fun c d ->
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok newenv -> d newenv

let run : 'a ea_result -> 'a result =
  fun c ->
  c EmptyEnv

let lookup : env ea_result = fun env ->
  Ok env

let rec sequence : 'a ea_result list -> 'a list ea_result =
  fun l ->
  match l with
  | [] -> return []
  | h::t ->
    h >>= fun ev ->
    sequence t >>= fun evs ->
    return (ev::evs)
      
(* Operations on environments *)

let empty_env : unit -> env ea_result = fun () ->
  return EmptyEnv

let extend_env : string -> exp_val -> env ea_result = fun id v env ->
  Ok (ExtendEnv(id,v,env))

let rec extend_env_list_helper =
  fun ids evs en ->
  match ids,evs with
  | [],[] -> en
  | id::idt,ev::evt ->
    ExtendEnv(id,ev,extend_env_list_helper idt evt en)
  | _,_ -> failwith
             "extend_env_list_helper: ids and evs have different sizes"
  
let extend_env_list =
  fun ids evs ->
  fun en ->
  Ok (extend_env_list_helper ids evs en)
    
let rec apply_env : string -> exp_val ea_result = fun id env ->
  match env with
  | EmptyEnv -> Error (id^" not found!")
  | ExtendEnv(v,ev,tail) ->
    if id=v
    then Ok ev
    else apply_env id tail



(* operations on expressed values *)

let int_of_numVal : exp_val -> int ea_result =  function
  |  NumVal n -> return n
  | _ -> error "Expected a number!"

let bool_of_boolVal : exp_val -> bool ea_result =  function
  |  BoolVal b -> return b
  | _ -> error "Expected a boolean!"

let list_of_tupleVal : exp_val -> (exp_val list)  ea_result =  function
  |  TupleVal l -> return l
  | _ -> error "Expected a tuple!"
           
let pair_of_pairVal : exp_val -> (exp_val*exp_val) ea_result =  function
  |  PairVal(ev1,ev2) -> return (ev1,ev2)
  | _ -> error "Expected a pair!"

let tree_of_treeVal : exp_val -> (exp_val tree) ea_result =  function
  |  TreeVal t -> return t
  | _ -> error "Expected a tree!"

let record_of_recordVal : exp_val -> ((string*exp_val) list) ea_result =  function 
  |  RecordVal r -> return r
  | _ -> error "Expected a record!"

let proj_of_projVal : exp_val -> (exp_val*string) ea_result =  function
  |  ProjVal(ev,s) -> return (ev,s)
  | _ -> error "Expected a proj!"

let rec string_of_expval = function
  | NumVal n -> "NumVal " ^ string_of_int n
  | BoolVal b -> "BoolVal " ^ string_of_bool b
  | PairVal (ev1,ev2) -> "PairVal("^string_of_expval ev1
                         ^","^ string_of_expval ev2^")"
  | TupleVal evs -> "TupleVal("^String.concat "," (List.map string_of_expval evs)^")"
  | _ -> "Not implemented"

let rec string_of_env' ac = function
  | EmptyEnv ->  "["^String.concat ",\n" ac^"]"
  | ExtendEnv(id,v,env) -> string_of_env' ((id^":="^string_of_expval v)::ac) env

let string_of_env : string ea_result =
  fun env ->
  match env with
  | EmptyEnv -> Ok ">>Environment:\nEmpty"
  | _ -> Ok (">>Environment:\n"^ string_of_env' [] env)


let rec has_duplicates = function
  | [] -> false
  | h::t -> List.mem h t || has_duplicates t


let eval_field_expr f (field, (_, e)) =
  f e >>= fun ex ->
    return (field, ex)

let rec eval_fields f fs =
  match fs with
  | [] -> return []
  | h::t ->
    eval_field_expr f h >>= fun ev ->
    eval_fields f t >>= fun evs ->
    return (ev::evs)


let rec proj_helper fs id =
  match fs with
  | [] -> error "Proj: field does not exist"
  | (f,v)::t -> 
    if f = id 
      then return v
    else
      proj_helper t id

