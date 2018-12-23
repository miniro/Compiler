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

and expr =   
  | Pi     
  | Ftoi   of expr           
  | Itof   of expr                                             
  | Access of access              
  | Assign of access * expr        
  | Addr of access                 
  | CstI of int
  | CstS of string
  | CstBIN of int
  | CstOCT of int
  | CstHEX of int
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
  | Sort of int list
  | Max of expr * expr
  | Min of expr * expr
  | Swap of access * access
  | Abs of expr
  | Round of expr
  | Floor of expr
  | Ceil of expr
  | Gcd of expr * expr
  | Mcm of expr * expr
  | Sin of expr
  | Cos of expr
  | Tan of expr
  | Asin of expr
  | Acos of expr
  | Atan of expr
  | Fabs of expr
  | Sqrt of expr
  | Log of expr
  | Pow of expr * expr
and stmt =
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Do of stmt * expr
  | For of expr * expr * expr * stmt
  | Expr of expr
  | Return of expr option
  | Block of stmtordec list
  | Switch of expr * (int * stmt) list

and stmtordec =
  | Dec of typ * string
  | Stmt of stmt

and topdec = 
  | Fundec of typ option * string * (typ * string) list * stmt
  | Vardec of typ * string

and program = 
  | Prog of topdec list