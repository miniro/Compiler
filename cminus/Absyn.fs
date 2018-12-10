module Absyn

type typ =
  | TypI                           
  | TypC                           
  | TypA of typ * int option      
  | TypP of typ                    

and access =                                                      
  | AccVar of string                 
  | AccDeref of expr                
  | AccIndex of access * expr

and expr =                                                         
  | Access of access              
  | Assign of access * expr        
  | Addr of access                 
  | CstI of int                      
  | Prim1 of string * expr
  | Prim2 of string * expr * expr
  | Prim3 of expr * expr * expr 
  | P1 of access * string   //用于处理'++a,--a'的情况
  | P2 of string * access   //用于处理'a++,a--'的情况
  | Andalso of expr * expr          
  | Orelse of expr * expr           
  | Call of string * expr list       

and stmt =
  | If of expr * stmt * stmt
  | Switch of expr * (expr * stmt) list * stmt
  | While of expr * stmt
  | Do of stmt * expr
  | Expr of expr
  | Return of expr option
  | Block of stmtordec list

and stmtordec =
  | Dec of typ * string
  | Stmt of stmt

and topdec = 
  | Fundec of typ option * string * (typ * string) list * stmt
  | Vardec of typ * string

and program = 
  | Prog of topdec list