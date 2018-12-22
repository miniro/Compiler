// Signature file for parser generated by fsyacc
module CPar
type token = 
  | EOF
  | PLUSASSIGN
  | MINUSASSIGN
  | TIMESASSIGN
  | DIVASSIGN
  | MODASSIGN
  | COLON
  | QUESTION
  | BITAND
  | BITOR
  | BITXOR
  | BITLEFT
  | BITRIGHT
  | BITANDASSIGN
  | BITORASSIGN
  | BITXORASSIGN
  | BITLEFTASSIGN
  | BITRIGHTASSIGN
  | BITNOT
  | LPAR
  | RPAR
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | SEMI
  | COMMA
  | ASSIGN
  | AMP
  | NOT
  | SEQOR
  | SEQAND
  | EQ
  | NE
  | GT
  | LT
  | GE
  | LE
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | PLUSONE
  | MINUSONE
  | INVO
  | COS
  | SIN
  | TAN
  | ASIN
  | ACOS
  | ATAN
  | CHAR
  | ELSE
  | IF
  | INT
  | FLOAT
  | NULL
  | PRINT
  | PRINTI
  | PRINTC
  | PRINTF
  | PRINTLN
  | RETURN
  | VOID
  | WHILE
  | DO
  | FOR
  | DEFAULT
  | SORT
  | MAX
  | MIN
  | SWAP
  | ABS
  | SWITCH
  | CASE
  | GCD
  | MCM
  | ROUND
  | FLOOR
  | CEIL
  | FTOI
  | ITOF
  | PI
  | FABS
  | LOG
  | SQRT
  | POW
  | CSTSTRING of (string)
  | NAME of (string)
  | CSTFLOAT of (float)
  | CSTINT of (int)
  | CSTBOOL of (int)
  | CSTHEX of (int)
  | CSTOCT of (int)
  | CSTBIN of (int)
type tokenId = 
    | TOKEN_EOF
    | TOKEN_PLUSASSIGN
    | TOKEN_MINUSASSIGN
    | TOKEN_TIMESASSIGN
    | TOKEN_DIVASSIGN
    | TOKEN_MODASSIGN
    | TOKEN_COLON
    | TOKEN_QUESTION
    | TOKEN_BITAND
    | TOKEN_BITOR
    | TOKEN_BITXOR
    | TOKEN_BITLEFT
    | TOKEN_BITRIGHT
    | TOKEN_BITANDASSIGN
    | TOKEN_BITORASSIGN
    | TOKEN_BITXORASSIGN
    | TOKEN_BITLEFTASSIGN
    | TOKEN_BITRIGHTASSIGN
    | TOKEN_BITNOT
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_LBRACE
    | TOKEN_RBRACE
    | TOKEN_LBRACK
    | TOKEN_RBRACK
    | TOKEN_SEMI
    | TOKEN_COMMA
    | TOKEN_ASSIGN
    | TOKEN_AMP
    | TOKEN_NOT
    | TOKEN_SEQOR
    | TOKEN_SEQAND
    | TOKEN_EQ
    | TOKEN_NE
    | TOKEN_GT
    | TOKEN_LT
    | TOKEN_GE
    | TOKEN_LE
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_TIMES
    | TOKEN_DIV
    | TOKEN_MOD
    | TOKEN_PLUSONE
    | TOKEN_MINUSONE
    | TOKEN_INVO
    | TOKEN_COS
    | TOKEN_SIN
    | TOKEN_TAN
    | TOKEN_ASIN
    | TOKEN_ACOS
    | TOKEN_ATAN
    | TOKEN_CHAR
    | TOKEN_ELSE
    | TOKEN_IF
    | TOKEN_INT
    | TOKEN_FLOAT
    | TOKEN_NULL
    | TOKEN_PRINT
    | TOKEN_PRINTI
    | TOKEN_PRINTC
    | TOKEN_PRINTF
    | TOKEN_PRINTLN
    | TOKEN_RETURN
    | TOKEN_VOID
    | TOKEN_WHILE
    | TOKEN_DO
    | TOKEN_FOR
    | TOKEN_DEFAULT
    | TOKEN_SORT
    | TOKEN_MAX
    | TOKEN_MIN
    | TOKEN_SWAP
    | TOKEN_ABS
    | TOKEN_SWITCH
    | TOKEN_CASE
    | TOKEN_GCD
    | TOKEN_MCM
    | TOKEN_ROUND
    | TOKEN_FLOOR
    | TOKEN_CEIL
    | TOKEN_FTOI
    | TOKEN_ITOF
    | TOKEN_PI
    | TOKEN_FABS
    | TOKEN_LOG
    | TOKEN_SQRT
    | TOKEN_POW
    | TOKEN_CSTSTRING
    | TOKEN_NAME
    | TOKEN_CSTFLOAT
    | TOKEN_CSTINT
    | TOKEN_CSTBOOL
    | TOKEN_CSTHEX
    | TOKEN_CSTOCT
    | TOKEN_CSTBIN
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startMain
    | NONTERM_Main
    | NONTERM_Topdecs
    | NONTERM_Topdec
    | NONTERM_Vardec
    | NONTERM_Vardesc
    | NONTERM_Fundec
    | NONTERM_Paramdecs
    | NONTERM_Paramdecs1
    | NONTERM_Block
    | NONTERM_StmtOrDecSeq
    | NONTERM_Stmt
    | NONTERM_StmtM
    | NONTERM_StmtU
    | NONTERM_Caselist
    | NONTERM_Casedec
    | NONTERM_Expr
    | NONTERM_ExprNotAccess
    | NONTERM_AtExprNotAccess
    | NONTERM_Access
    | NONTERM_Exprs
    | NONTERM_Exprs1
    | NONTERM_Const
    | NONTERM_ConstHEX
    | NONTERM_ConstOCT
    | NONTERM_ConstBIN
    | NONTERM_ConstF
    | NONTERM_Type
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val Main : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (Absyn.program) 
