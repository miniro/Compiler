module Comp

open System.IO
open Absyn
open Machine

(* ------------------------------------------------------------------- *)

(* Simple environment operations *)

type 'data Env = (string * 'data) list

let rec lookup env x = 
    match env with 
    | []         -> failwith (x + " not found")
    | (y, v)::yr -> if x=y then v else lookup yr x

(* A global variable has an absolute address, a local one has an offset: *)

type Var = 
     | Glovar of int                   (* absolute address in stack           *)
     | Locvar of int                   (* address relative to bottom of frame *)

(* The variable environment keeps track of global and local variables, and 
   keeps track of next available offset for local variables *)

type VarEnv = (Var * typ) Env * int

(* The function environment maps function name to label and parameter decs *)

type Paramdecs = (typ * string) list
type FunEnv = (label * typ option * Paramdecs) Env

(* Bind declared variable in env and generate code to allocate it: *)

let allocate (kind : int -> Var) (typ, x) (varEnv : VarEnv) : VarEnv * instr list =
    printf "allocate called!\n"      
    let (env, fdepth) = varEnv 
    match typ with
    | TypA (TypA _, _) -> 
      raise (Failure "allocate: array of arrays not permitted")
    | TypA (t, Some i) -> 
      let newEnv = ((x, (kind (fdepth+i), typ)) :: env, fdepth+i+1) //数组占用 i个位置
      let code = [INCSP i; GETSP; CSTI (i-1); SUB]
      (newEnv, code)
    | _ -> 
      let newEnv = ((x, (kind (fdepth), typ)) :: env, fdepth+1)
      let code = [INCSP 1]

      printf "new varEnv: %A\n" newEnv // 调试 显示分配后环境变化
      (newEnv, code)

(* Bind declared parameters in env: *)

let bindParam (env, fdepth) (typ, x)  : VarEnv = 
    ((x, (Locvar fdepth, typ)) :: env , fdepth+1)

let bindParams paras ((env, fdepth) : VarEnv) : VarEnv = 
    List.fold bindParam (env, fdepth) paras;

(* ------------------------------------------------------------------- *)

(* Build environments for global variables and functions *)

let makeGlobalEnvs (topdecs : topdec list) : VarEnv * FunEnv * instr list = 
    let rec addv decs varEnv funEnv = 
        printf "Global funEnv: %A\n" funEnv  
        match decs with 
        | []         -> (varEnv, funEnv, [])
        | dec::decr  -> 
          match dec with
          | Vardec (typ, var) ->
            let (varEnv1, code1)        = allocate Glovar (typ, var) varEnv
            let (varEnvr, funEnvr, coder) = addv decr varEnv1 funEnv
            (varEnvr, funEnvr, code1 @ coder)
          | Fundec (tyOpt, f, xs, body) ->
            addv decr varEnv ((f, (newLabel(), tyOpt, xs)) :: funEnv)
    addv topdecs ([], 0) []

(* ------------------------------------------------------------------- *)

(* Compiling micro-C statements: 
   * stmt    is the statement to compile
   * varenv  is the local and global variable environment 
   * funEnv  is the global function environment
*)

let rec cStmt stmt (varEnv : VarEnv) (funEnv : FunEnv) : instr list = 


    match stmt with
    | If (e, stmt1, stmt2) -> 
      let labelse = newLabel()
      let labend  = newLabel()
      cExpr e varEnv funEnv @ [IFZERO labelse] 
      @ cStmt stmt1 varEnv funEnv @ [GOTO labend]
      @ [Label labelse] @ cStmt stmt2 varEnv funEnv
      @ [Label labend]           
    | While(e, body) ->
      let labbegin = newLabel()
      let labtest  = newLabel()
      [GOTO labtest; Label labbegin] @ cStmt body varEnv funEnv
      @ [Label labtest] @ cExpr e varEnv funEnv @ [IFNZRO labbegin]
    | Do(body, e) ->
      let labbegin = newLabel()
      let labtest  = newLabel()
      [Label labbegin] @ cStmt body varEnv funEnv
      @ [Label labtest] @ cExpr e varEnv funEnv @ [IFNZRO labbegin]
    | For(e1, e2, e3, body) ->
      let labbegin = newLabel()
      let labtest  = newLabel()
      cExpr e1 varEnv funEnv @ [INCSP -1] @ [GOTO labtest; Label labbegin]
      @ cStmt body varEnv funEnv @ cExpr e3 varEnv funEnv @ [INCSP -1]
      @ [Label labtest] @ cExpr e2 varEnv funEnv @ [IFNZRO labbegin]
    | Expr e -> 
      cExpr e varEnv funEnv @ [INCSP -1]
    | Block stmts -> 
      
      let rec loop stmts varEnv =
          match stmts with 
          | []     -> (snd varEnv, [])
          | s1::sr -> 
            let (varEnv1, code1) = cStmtOrDec s1 varEnv funEnv
            let (fdepthr, coder) = loop sr varEnv1 
            (fdepthr, code1 @ coder)
      
      let (fdepthend, code) = loop stmts varEnv

      code @ [INCSP(snd varEnv - fdepthend)]
      
    | Return None -> 
      [RET (snd varEnv - 1)]
    | Return (Some e) -> 
      cExpr e varEnv funEnv @ [RET (snd varEnv)]

and cStmtOrDec stmtOrDec (varEnv : VarEnv) (funEnv : FunEnv) : VarEnv * instr list = 
    match stmtOrDec with 
    | Stmt stmt    -> (varEnv, cStmt stmt varEnv funEnv) 
    | Dec (typ, x) -> allocate Locvar (typ, x) varEnv

and cExpr (e : expr) (varEnv : VarEnv) (funEnv : FunEnv) : instr list = 
    match e with
    | Access acc     -> cAccess acc varEnv funEnv @ [LDI] 
    | Assign(acc, e) -> cAccess acc varEnv funEnv @ cExpr e varEnv funEnv @ [STI]
    | CstI i         -> [CSTI i]
    | Addr acc       -> cAccess acc varEnv funEnv
    | P1(acc, ope)   -> 
      cAccess acc varEnv funEnv @ [DUP] @ [LDI] @ [CSTI 1]
      @ (match ope with
        | "+" -> [ADD]
        | "-" -> [SUB] 
        | _   -> raise (Failure "unknown primitive 1")) @ [STI]
    | P2(ope, acc)   ->
      cAccess acc varEnv funEnv @ [DUP] @ [LDI] @ [CSTI 1]
      @ (match ope with
        | "+" -> [ADD] @ [STI] @ [CSTI 1] @ [SUB]
        | "-" -> [SUB] @ [STI] @ [CSTI 1] @ [ADD]
        | _   -> raise (Failure "unknown primitive 2"))
    | A(ope, acc, e) ->
      cAccess acc varEnv funEnv @ [DUP] @ [LDI] @ cExpr e varEnv funEnv
      @ (match ope with
        | "+" -> [ADD]
        | "-" -> [SUB]
        | "*" -> [MUL]
        | "/" -> [DIV]
        | "%" -> [MOD] 
        | _   -> raise (Failure "unknown primitive 3")) @ [STI]
    | Prim1(ope, e1) ->
      cExpr e1 varEnv funEnv
      @ (match ope with
         | "!"      -> [NOT]
         | "printi" -> [PRINTI]
         | "printc" -> [PRINTC]
         | _        -> raise (Failure "unknown primitive 4"))
    | Prim2(ope, e1, e2) ->
      cExpr e1 varEnv funEnv
      @ cExpr e2 varEnv funEnv
      @ (match ope with
         | "*"   -> [MUL]
         | "+"   -> [ADD]
         | "-"   -> [SUB]
         | "/"   -> [DIV]
         | "%"   -> [MOD]
         | "=="  -> [EQ]
         | "!="  -> [EQ; NOT]
         | "<"   -> [LT]
         | ">="  -> [LT; NOT]
         | ">"   -> [SWAP; LT]
         | "<="  -> [SWAP; LT; NOT]
         | _     -> raise (Failure "unknown primitive 5"))
    | Andalso(e1, e2) ->
      let labend   = newLabel()
      let labfalse = newLabel()
      cExpr e1 varEnv funEnv
      @ [IFZERO labfalse]
      @ cExpr e2 varEnv funEnv
      @ [GOTO labend; Label labfalse; CSTI 0; Label labend]            
    | Orelse(e1, e2) -> 
      let labend  = newLabel()
      let labtrue = newLabel()
      cExpr e1 varEnv funEnv
      @ [IFNZRO labtrue]
      @ cExpr e2 varEnv funEnv
      @ [GOTO labend; Label labtrue; CSTI 1; Label labend]
    | Call(f, es) -> callfun f es varEnv funEnv
    | Question(e1, e2, e3)  ->
      let labtrue = newLabel()
      let labelend = newLabel()
      cExpr e1 varEnv funEnv @ [IFNZRO labtrue]
      @ cExpr e3 varEnv funEnv @ [GOTO labelend]
      @ [Label labtrue] @ cExpr e2 varEnv funEnv
      @ [Label labelend]
    | _     -> raise (Failure "unknown primitive 6")
    
      

and cAccess access varEnv funEnv : instr list =
    match access with 
    | AccVar x ->
      match lookup (fst varEnv) x with
      | Glovar addr, _ -> [CSTI addr]
      | Locvar addr, _ -> [GETBP; CSTI addr; ADD]
    | AccDeref e -> cExpr e varEnv funEnv
    | AccIndex(acc, idx) -> cAccess acc varEnv funEnv 
                            @ [LDI] @ cExpr idx varEnv funEnv @ [ADD]

and cExprs es varEnv funEnv : instr list = 
    List.concat(List.map (fun e -> cExpr e varEnv funEnv) es)

(* Generate code to evaluate arguments es and then call function f: *)
    
and callfun f es varEnv funEnv : instr list =
    let (labf, tyOpt, paramdecs) = lookup funEnv f
    let argc = List.length es
    if argc = List.length paramdecs then
      cExprs es varEnv funEnv @ [CALL(argc, labf)]
    else
      raise (Failure (f + ": parameter/argument mismatch"))


(* Compile a complete micro-C program: globals, call to main, functions *)

let cProgram (Prog topdecs) : instr list = 
    let _ = resetLabels ()
    let ((globalVarEnv, _), funEnv, globalInit) = makeGlobalEnvs topdecs
    let compilefun (tyOpt, f, xs, body) =
        let (labf, _, paras) = lookup funEnv f
        let (envf, fdepthf) = bindParams paras (globalVarEnv, 0)
        let code = cStmt body (envf, fdepthf) funEnv
        [Label labf] @ code @ [RET (List.length paras-1)]
    let functions = 
        List.choose (function 
                         | Fundec (rTy, name, argTy, body) 
                                    -> Some (compilefun (rTy, name, argTy, body))
                         | Vardec _ -> None)
                    topdecs 
    let (mainlab, _, mainparams) = lookup funEnv "main"
    let argc = List.length mainparams
    globalInit 
    @ [LDARGS; CALL(argc, mainlab); STOP] 
    @ List.concat functions

(* Compile a complete micro-C and write the resulting instruction list
   to file fname; also, return the program as a list of instructions.
 *)

let intsToFile (inss : int list) (fname : string) = 
    File.WriteAllText(fname, String.concat " " (List.map string inss))

let compileToFile program fname = 
    let instrs   = cProgram program 
    // printf "instrs: %A\n" instrs

    let bytecode = code2ints instrs
    // printf "bytecode: %A\n" bytecode 

    // let deinstrs = decomp bytecode
    // printf "deinstrs: %A\n" deinstrs

    intsToFile bytecode fname
    instrs

(* Example programs are found in the files ex1.c, ex2.c, etc *)