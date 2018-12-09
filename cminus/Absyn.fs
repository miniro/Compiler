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
  | Andalso of expr * expr          
  | Orelse of expr * expr           
  | Call of string * expr list       

and stmt =
  | If of expr * stmt * stmt
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