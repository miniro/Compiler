module Absyn

type typ =
  | TypI                           
  | TypC                           
  | TypA of typ * int option      
  | TypP of typ
  | TypF 

and access =                                                      
  | AccVar of string                 
  | AccDeref of expr                
  | AccIndex of access * expr

and buildfun =
  | Sort of int list
  | Max of int list
  | Min of int list
  | Swap of int * int

and expr =                                                         
  | Access of access              
  | Assign of access * expr        
  | Addr of access                 
  | CstI of int
  | CstF of float                      
  | Prim1 of string * expr
  | Prim2 of string * expr * expr
  | Prim3 of expr * expr * expr 
  | Bitassign of string * access * expr
  | P1 of access * string   //用于处理'++a,--a'的情况
  | P2 of string * access   //用于处理'a++,a--'的情况
  | A of string * access * expr
  | Andalso of expr * expr          
  | Orelse of expr * expr     
  | Bitxor of expr * expr  
  | Bitand of expr * expr  
  | Bitor of expr * expr
  | Bitleft of expr * expr
  | Bitright of expr * expr 
  | Bitnot of string * expr 
  | Call of string * expr list
  | Question of expr * expr * expr
  // | Buildfun of buildfun

and stmt =
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Do of stmt * expr
  | For of expr * expr * expr * stmt
  | Expr of expr
  | Return of expr option
  | Block of stmtordec list
  | Switch of expr * (expr * stmt) list
  | Case of expr * stmt

and stmtordec =
  | Dec of typ * string
  | Stmt of stmt

and topdec = 
  | Fundec of typ option * string * (typ * string) list * stmt
  | Vardec of typ * string

and program = 
  | Prog of topdec list