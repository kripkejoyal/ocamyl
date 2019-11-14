/*  Yacc grammar for the parser. */

%{
open Support.Error
open Support.Pervasive
open Syntax
open Core 
open Format
open Type
open Eval

let pe = print_endline 
%}

/* All token has info type 
 * So the declaration of a token is;
 *  X:  %token IF
 *  O:  %token <info> IF 
 * and sometime, -- in the case of identifiers and 
 * constant values -- more info is provided. */

/* Keyword tokens */
%token <Support.Error.info> BOOL
%token <Support.Error.info> NAT

%token <Support.Error.info> LAMBDA
%token <Support.Error.info> IF
%token <Support.Error.info> THEN
%token <Support.Error.info> ELSE
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE
%token <Support.Error.info> SUCC
%token <Support.Error.info> PRED
%token <Support.Error.info> ISZERO

/* Identifier and constant value tokens */
%token <string  Support.Error.withinfo> UCID  /* uppercase-initial */
%token <string  Support.Error.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int     Support.Error.withinfo> INTV
%token <float   Support.Error.withinfo> FLOATV
%token <string  Support.Error.withinfo> STRINGV

/* Symbolic tokens */
%token <Support.Error.info> APOSTROPHE
%token <Support.Error.info> DQUOTE
%token <Support.Error.info> ARROW
%token <Support.Error.info> BANG
%token <Support.Error.info> BARGT
%token <Support.Error.info> BARRCURLY
%token <Support.Error.info> BARRSQUARE
%token <Support.Error.info> COLON
%token <Support.Error.info> COLONCOLON
%token <Support.Error.info> COLONEQ
%token <Support.Error.info> COLONHASH
%token <Support.Error.info> COMMA
%token <Support.Error.info> DARROW
%token <Support.Error.info> DDARROW
%token <Support.Error.info> DOT
%token <Support.Error.info> EOF
%token <Support.Error.info> EQ
%token <Support.Error.info> EQEQ
%token <Support.Error.info> EXISTS
%token <Support.Error.info> GT
%token <Support.Error.info> HASH
%token <Support.Error.info> LCURLY
%token <Support.Error.info> LCURLYBAR
%token <Support.Error.info> LEFTARROW
%token <Support.Error.info> LPAREN
%token <Support.Error.info> LSQUARE
%token <Support.Error.info> LSQUAREBAR
%token <Support.Error.info> LT
%token <Support.Error.info> RCURLY
%token <Support.Error.info> RPAREN
%token <Support.Error.info> RSQUARE
%token <Support.Error.info> SEMI        /* semicolon */ 
%token <Support.Error.info> SLASH
%token <Support.Error.info> STAR
%token <Support.Error.info> TRIANGLE
%token <Support.Error.info> USCORE
%token <Support.Error.info> VBAR
%token <Support.Error.info> NEWLINE
%token <Support.Error.info> DOUBLESEMI
%token <Support.Error.info> HOGE
/* The returned type of a toplevel is Syntax.command list. */
%start toplevel
%start input 
%type <Syntax.context -> (Syntax.command list * Syntax.context)> input 
%type <Syntax.context -> (Syntax.command list * Syntax.context)> toplevel

%%
/* Begin Interpreter */ 
input : /* Left Recursion */
    |                               { fun ctx   -> [],ctx  }
    | input block                   { fun ctx   -> 
                                        let blks,ctx = $1 ctx in 
                                        let blk,ctx = $2 ctx in 
                                        (List.append blks blk, ctx) } 
block :     /* Left Recursion */                    
    | Command                       { fun ctx   -> let cmd,ctx = $1 ctx in [cmd],ctx }             
    | block SEMI Command            { fun ctx   -> 
                                        let cmd,ctx = $3 ctx in 
                                        let cmds,ctx  = $1 ctx in 
                                        List.append cmds [cmd], ctx }  
    | block HOGE                    { let cmds,ctx = $1 emptycontext in let _ = List.iter (print_eval ctx) cmds in 
                                      fun ctx   -> let blk,ctx = $1 ctx in blk,ctx }  
    | block EOF                     { fun ctx   -> let blk,ctx = $1 ctx in blk,ctx } 
/* End Interpreter */

/* Begin Compiler */
toplevel :  /* Right Recursion */                
    | EOF                           { fun ctx   -> [],ctx } 
    | Command SEMI toplevel         { fun ctx   -> 
          let cmd,ctx   = $1 ctx in 
          let cmds,ctx  = $3 ctx in 
          cmd::cmds,ctx } 
/* End Compliler */ 

/* Modules both for Interpreter and for Compiler */ 
Command     :       /* A top-level command */ 
    | Term                          { fun ctx   -> let t = $1 ctx in Eval(tmInfo t,t),ctx }
    | LCID Binder                   { fun ctx   -> ((Bind($1.i,$1.v,$2 ctx)), addname ctx $1.v) } 
Binder      : 
    | COLON Type                    { fun ctx   -> VarBind($2 ctx) } 

Type        : 
    | ArrowType                     { $1 } 
AType       : 
    | LPAREN Type RPAREN            { $2 } 
    | BOOL                          { fun ctx   -> TyBool } 
    | NAT                           { fun ctx   -> TyNat  }
ArrowType   :
    | AType ARROW ArrowType         { fun ctx   -> TyArr($1 ctx, $3 ctx) }
    | AType                         { $1 } 

Term        :
    | AppTerm                       { $1 }
    | LAMBDA LCID COLON Type DOT Term   
        { pe "PARSER: λx:T.t"; fun ctx -> let ctx1=addname ctx $2.v in TmAbs($1,$2.v,$4 ctx,$6 ctx1)}
    | IF Term THEN Term ELSE Term   { fun ctx   -> TmIf($1, $2 ctx, $4 ctx, $6 ctx) }
AppTerm     :
    | ATerm                         { $1 }
    | SUCC ATerm                    { fun ctx   -> TmSucc($1, $2 ctx ) }
    | PRED ATerm                    { fun ctx   -> TmPred($1, $2 ctx ) }
    | ISZERO ATerm                  { fun ctx   -> TmIsZero($1, $2 ctx) }
    | AppTerm ATerm                 { fun ctx   -> let e1=$1 ctx in TmApp(tmInfo e1,e1,$2 ctx) }
ATerm       :         /* Atomic terms are ones that never require extra parentheses */
    | LPAREN Term RPAREN            { pe "PARSER: ( t )"; $2 } 
    | LCID                          { fun ctx   -> TmVar($1.i, name2index $1.i ctx $1.v, ctxlength ctx) } 
    | TRUE                          { fun ctx   -> TmTrue($1) }
    | FALSE                         { fun ctx   -> TmFalse($1) }
    | INTV                          { fun ctx   -> let rec f = function
              0 -> TmZero($1.i)
            | n -> pe "succ"; TmSucc($1.i, f (n-1))
          in f $1.v }


