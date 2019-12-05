type token =
  | BOOL of (Support.Error.info)
  | NAT of (Support.Error.info)
  | LAMBDA of (Support.Error.info)
  | IF of (Support.Error.info)
  | THEN of (Support.Error.info)
  | ELSE of (Support.Error.info)
  | TRUE of (Support.Error.info)
  | FALSE of (Support.Error.info)
  | SUCC of (Support.Error.info)
  | PRED of (Support.Error.info)
  | ISZERO of (Support.Error.info)
  | UCID of (string  Support.Error.withinfo)
  | LCID of (string  Support.Error.withinfo)
  | INTV of (int     Support.Error.withinfo)
  | FLOATV of (float   Support.Error.withinfo)
  | STRINGV of (string  Support.Error.withinfo)
  | APOSTROPHE of (Support.Error.info)
  | DQUOTE of (Support.Error.info)
  | ARROW of (Support.Error.info)
  | BANG of (Support.Error.info)
  | BARGT of (Support.Error.info)
  | BARRCURLY of (Support.Error.info)
  | BARRSQUARE of (Support.Error.info)
  | COLON of (Support.Error.info)
  | COLONCOLON of (Support.Error.info)
  | COLONEQ of (Support.Error.info)
  | COLONHASH of (Support.Error.info)
  | COMMA of (Support.Error.info)
  | DARROW of (Support.Error.info)
  | DDARROW of (Support.Error.info)
  | DOT of (Support.Error.info)
  | EOF of (Support.Error.info)
  | EQ of (Support.Error.info)
  | EQEQ of (Support.Error.info)
  | EXISTS of (Support.Error.info)
  | GT of (Support.Error.info)
  | HASH of (Support.Error.info)
  | LCURLY of (Support.Error.info)
  | LCURLYBAR of (Support.Error.info)
  | LEFTARROW of (Support.Error.info)
  | LPAREN of (Support.Error.info)
  | LSQUARE of (Support.Error.info)
  | LSQUAREBAR of (Support.Error.info)
  | LT of (Support.Error.info)
  | RCURLY of (Support.Error.info)
  | RPAREN of (Support.Error.info)
  | RSQUARE of (Support.Error.info)
  | SEMI of (Support.Error.info)
  | SLASH of (Support.Error.info)
  | STAR of (Support.Error.info)
  | TRIANGLE of (Support.Error.info)
  | USCORE of (Support.Error.info)
  | VBAR of (Support.Error.info)
  | NEWLINE of (Support.Error.info)
  | DOUBLESEMI of (Support.Error.info)
  | HOGE of (Support.Error.info)

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Support.Error
open Support.Pervasive
open Syntax
open Core 
open Format
open Type
open Eval

let pe = print_endline 
# 72 "parser.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
  257 (* BOOL *);
  258 (* NAT *);
  259 (* LAMBDA *);
  260 (* IF *);
  261 (* THEN *);
  262 (* ELSE *);
  263 (* TRUE *);
  264 (* FALSE *);
  265 (* SUCC *);
  266 (* PRED *);
  267 (* ISZERO *);
  268 (* UCID *);
  269 (* LCID *);
  270 (* INTV *);
  271 (* FLOATV *);
  272 (* STRINGV *);
  273 (* APOSTROPHE *);
  274 (* DQUOTE *);
  275 (* ARROW *);
  276 (* BANG *);
  277 (* BARGT *);
  278 (* BARRCURLY *);
  279 (* BARRSQUARE *);
  280 (* COLON *);
  281 (* COLONCOLON *);
  282 (* COLONEQ *);
  283 (* COLONHASH *);
  284 (* COMMA *);
  285 (* DARROW *);
  286 (* DDARROW *);
  287 (* DOT *);
    0 (* EOF *);
  288 (* EQ *);
  289 (* EQEQ *);
  290 (* EXISTS *);
  291 (* GT *);
  292 (* HASH *);
  293 (* LCURLY *);
  294 (* LCURLYBAR *);
  295 (* LEFTARROW *);
  296 (* LPAREN *);
  297 (* LSQUARE *);
  298 (* LSQUAREBAR *);
  299 (* LT *);
  300 (* RCURLY *);
  301 (* RPAREN *);
  302 (* RSQUARE *);
  303 (* SEMI *);
  304 (* SLASH *);
  305 (* STAR *);
  306 (* TRIANGLE *);
  307 (* USCORE *);
  308 (* VBAR *);
  309 (* NEWLINE *);
  310 (* DOUBLESEMI *);
  311 (* HOGE *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\003\000\003\000\001\000\001\000\004\000\
\004\000\006\000\007\000\009\000\009\000\009\000\008\000\008\000\
\005\000\005\000\005\000\010\000\010\000\010\000\010\000\010\000\
\011\000\011\000\011\000\011\000\011\000\000\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\002\000\003\000\001\000\003\000\001\000\
\002\000\002\000\001\000\003\000\001\000\001\000\003\000\001\000\
\001\000\006\000\006\000\001\000\002\000\002\000\002\000\002\000\
\003\000\001\000\001\000\001\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\001\000\000\000\000\000\000\000\027\000\028\000\000\000\
\000\000\000\000\000\000\029\000\006\000\000\000\030\000\000\000\
\008\000\000\000\020\000\000\000\000\000\026\000\000\000\021\000\
\022\000\023\000\000\000\009\000\000\000\000\000\024\000\002\000\
\003\000\000\000\000\000\000\000\013\000\014\000\000\000\010\000\
\011\000\000\000\025\000\007\000\000\000\004\000\000\000\000\000\
\000\000\000\000\005\000\000\000\000\000\012\000\015\000\018\000\
\019\000"

let yydgoto = "\003\000\
\015\000\020\000\033\000\016\000\017\000\028\000\040\000\041\000\
\042\000\018\000\019\000"

let yysindex = "\046\000\
\001\000\000\000\000\000\243\254\022\255\000\000\000\000\038\255\
\038\255\038\255\251\254\000\000\000\000\022\255\000\000\231\254\
\000\000\038\255\000\000\255\254\013\255\000\000\035\255\000\000\
\000\000\000\000\003\255\000\000\238\254\001\000\000\000\000\000\
\000\000\230\254\003\255\022\255\000\000\000\000\003\255\000\000\
\000\000\025\255\000\000\000\000\062\255\000\000\018\255\049\255\
\015\255\003\255\000\000\022\255\022\255\000\000\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\010\255\000\000\000\000\000\000\000\000\000\000\
\000\000\009\255\000\000\061\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\036\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\038\000\000\000\029\000\252\255\006\000\000\000\255\255\027\000\
\000\000\000\000\076\000"

let yytablesize = 297
let yytable = "\021\000\
\013\000\004\000\005\000\037\000\038\000\006\000\007\000\008\000\
\009\000\010\000\023\000\011\000\012\000\017\000\017\000\034\000\
\026\000\026\000\027\000\029\000\045\000\030\000\026\000\026\000\
\004\000\005\000\043\000\046\000\006\000\007\000\008\000\009\000\
\010\000\047\000\022\000\012\000\035\000\049\000\014\000\036\000\
\034\000\048\000\039\000\050\000\006\000\007\000\001\000\002\000\
\052\000\026\000\022\000\012\000\032\000\017\000\053\000\017\000\
\026\000\056\000\057\000\054\000\031\000\014\000\017\000\026\000\
\004\000\005\000\016\000\044\000\006\000\007\000\008\000\009\000\
\010\000\051\000\011\000\012\000\055\000\014\000\000\000\000\000\
\016\000\000\000\016\000\024\000\025\000\026\000\000\000\000\000\
\000\000\016\000\000\000\000\000\000\000\031\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\000\005\000\000\000\000\000\006\000\
\007\000\008\000\009\000\010\000\000\000\011\000\012\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\014\000"

let yycheck = "\013\001\
\000\000\003\001\004\001\001\001\002\001\007\001\008\001\009\001\
\010\001\011\001\005\000\013\001\014\001\005\001\006\001\020\000\
\007\001\008\001\024\001\014\000\047\001\047\001\013\001\014\001\
\003\001\004\001\045\001\054\001\007\001\008\001\009\001\010\001\
\011\001\035\000\013\001\014\001\024\001\039\000\040\001\005\001\
\045\000\036\000\040\001\019\001\007\001\008\001\001\000\002\000\
\031\001\040\001\013\001\014\001\054\001\045\001\006\001\047\001\
\047\001\052\000\053\000\045\001\000\000\040\001\054\001\054\001\
\003\001\004\001\031\001\030\000\007\001\008\001\009\001\010\001\
\011\001\045\000\013\001\014\001\050\000\040\001\255\255\255\255\
\045\001\255\255\047\001\008\000\009\000\010\000\255\255\255\255\
\255\255\054\001\255\255\255\255\255\255\018\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\040\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\004\001\255\255\255\255\007\001\
\008\001\009\001\010\001\011\001\255\255\013\001\014\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\040\001"

let yynames_const = "\
  "

let yynames_block = "\
  BOOL\000\
  NAT\000\
  LAMBDA\000\
  IF\000\
  THEN\000\
  ELSE\000\
  TRUE\000\
  FALSE\000\
  SUCC\000\
  PRED\000\
  ISZERO\000\
  UCID\000\
  LCID\000\
  INTV\000\
  FLOATV\000\
  STRINGV\000\
  APOSTROPHE\000\
  DQUOTE\000\
  ARROW\000\
  BANG\000\
  BARGT\000\
  BARRCURLY\000\
  BARRSQUARE\000\
  COLON\000\
  COLONCOLON\000\
  COLONEQ\000\
  COLONHASH\000\
  COMMA\000\
  DARROW\000\
  DDARROW\000\
  DOT\000\
  EOF\000\
  EQ\000\
  EQEQ\000\
  EXISTS\000\
  GT\000\
  HASH\000\
  LCURLY\000\
  LCURLYBAR\000\
  LEFTARROW\000\
  LPAREN\000\
  LSQUARE\000\
  LSQUAREBAR\000\
  LT\000\
  RCURLY\000\
  RPAREN\000\
  RSQUARE\000\
  SEMI\000\
  SLASH\000\
  STAR\000\
  TRIANGLE\000\
  USCORE\000\
  VBAR\000\
  NEWLINE\000\
  DOUBLESEMI\000\
  HOGE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
                                        ( fun _     ->  [],[]                                   )
# 334 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 94 "parser.mly"
                                        ( fun ctx   ->  [],ctx                                  )
# 342 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'oneREPL) in
    Obj.repr(
# 95 "parser.mly"
                                        ( let _,ev_ctx  = _1 [] in  
                                          let cmds,_    = _2 ev_ctx in 
                                          let ev_ctx'   = process_commands ev_ctx cmds in 
                                          fun _     ->  [],ev_ctx'                              )
# 353 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 100 "parser.mly"
                                        ( fun ctx   ->  let cmd,ctx'   = _1 ctx in [cmd],ctx'   )
# 361 "parser.ml"
               : 'oneREPL))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'oneREPL) in
    Obj.repr(
# 101 "parser.mly"
                                        ( fun ctx   ->  let cmd,ctx'   = _1 ctx in 
                                                        let cmds,ctx'' = _3 ctx' in cmd::cmds,ctx''  )
# 371 "parser.ml"
               : 'oneREPL))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 105 "parser.mly"
                                        ( fun ctx   ->  [],ctx                                  )
# 378 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    Obj.repr(
# 106 "parser.mly"
                                        ( fun ctx   ->  let cmd,ctx  = _1 ctx in 
                                                        let cmds,ctx = _3 ctx in cmd::cmds,ctx  )
# 388 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 110 "parser.mly"
                                    ( fun ctx   -> let t = _1 ctx in Eval(tmInfo t,t),ctx       )
# 395 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string  Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Binder) in
    Obj.repr(
# 111 "parser.mly"
                                    ( fun ctx   -> ((Bind(_1.i,_1.v,_2 ctx)), addname ctx _1.v) )
# 403 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 113 "parser.mly"
                                    ( fun ctx   -> VarBind(_2 ctx)  )
# 411 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 116 "parser.mly"
                                    ( _1 )
# 418 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 118 "parser.mly"
                                    ( _2 )
# 427 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 119 "parser.mly"
                                    ( fun ctx   -> TyBool )
# 434 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 120 "parser.mly"
                                    ( fun ctx   -> TyNat  )
# 441 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AType) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 122 "parser.mly"
                                    ( fun ctx   -> TyArr(_1 ctx, _3 ctx) )
# 450 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AType) in
    Obj.repr(
# 123 "parser.mly"
                                    ( _1 )
# 457 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 126 "parser.mly"
                                    ( _1 )
# 464 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string  Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 128 "parser.mly"
        ( pe "PARSER: λx:T.t"; fun ctx -> let ctx1=addname ctx _2.v in TmAbs(_1,_2.v,_4 ctx,_6 ctx1))
# 476 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 129 "parser.mly"
                                    ( fun ctx   -> TmIf(_1, _2 ctx, _4 ctx, _6 ctx) )
# 488 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 131 "parser.mly"
                                    ( _1 )
# 495 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 132 "parser.mly"
                                    ( fun ctx   -> TmSucc(_1, _2 ctx ) )
# 503 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 133 "parser.mly"
                                    ( fun ctx   -> TmPred(_1, _2 ctx ) )
# 511 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 134 "parser.mly"
                                    ( fun ctx   -> TmIsZero(_1, _2 ctx) )
# 519 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 135 "parser.mly"
                                    ( fun ctx   -> let e1=_1 ctx in TmApp(tmInfo e1,e1,_2 ctx) )
# 527 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 137 "parser.mly"
                                    ( pe "PARSER: ( t )"; _2 )
# 536 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string  Support.Error.withinfo) in
    Obj.repr(
# 138 "parser.mly"
                                    ( fun ctx   -> TmVar(_1.i, name2index _1.i ctx _1.v, ctxlength ctx) )
# 543 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 139 "parser.mly"
                                    ( fun ctx   -> TmTrue(_1) )
# 550 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 140 "parser.mly"
                                    ( fun ctx   -> TmFalse(_1) )
# 557 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int     Support.Error.withinfo) in
    Obj.repr(
# 141 "parser.mly"
                                    ( fun ctx   -> let rec f = function
              0 -> TmZero(_1.i)
            | n -> pe "succ"; TmSucc(_1.i, f (n-1))
          in f _1.v )
# 567 "parser.ml"
               : 'ATerm))
(* Entry toplevel *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry input *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let toplevel (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.context -> (Syntax.command list * Syntax.context))
let input (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Syntax.context -> (Syntax.command list * Syntax.context))
