module Absyn

type typ =
  | TypInt
  | TypDouble
  | TypBool
  | TypChar
  | TypLongLong
  | TypArray of typ * int option
  | TypPoint of typ

and access =                                                      
  | AccVar of string                 
  | AccDeref of expr                
  | AccIndex of access * expr        

and expr =                                                         
  | Access of access                
  | Assign of access * expr         
  | Addr of access                  
  | CstInt of int
  | CstDouble of double
  | CstBool of bool
  | CstChar of char          
  | Prim1 of string * expr          
  | Prim2 of string * expr * expr
  | Prim3 of string * expr * expr * expr 
  | Andalso of expr * expr          
  | Orelse of expr * expr          
  | Call of string * expr list      

and stmt =
  | If of expr * stmt * stmt
  | Switch of expr * (expr * stmt) list * stmt
  | While of expr * stmt
  | DoWhile of stmt * expr
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