{
open Support.Error

let reservedWords = [
  (* Keywords *)
  ("Bool",    fun i -> Parser.BOOL i);
  ("Nat",   fun i -> Parser.NAT i);
  ("\\",    fun i -> Parser.LAMBDA i);
  ("if",    fun i -> Parser.IF i);
  ("then",  fun i -> Parser.THEN i);
  ("else",  fun i -> Parser.ELSE i);
  ("true",  fun i -> Parser.TRUE i);
  ("false", fun i -> Parser.FALSE i);
  ("succ",  fun i -> Parser.SUCC i);
  ("pred",  fun i -> Parser.PRED i);
  ("iszero",fun i -> Parser.ISZERO i);
  
  (* Symbols *)
  ("_",     fun i -> Parser.USCORE i);
  ("'",     fun i -> Parser.APOSTROPHE i);
  ("\"",    fun i -> Parser.DQUOTE i);
  ("!",     fun i -> Parser.BANG i);
  ("#",     fun i -> Parser.HASH i);
  ("$",     fun i -> Parser.TRIANGLE i);
  ("*",     fun i -> Parser.STAR i);
  ("|",     fun i -> Parser.VBAR i);
  (".",     fun i -> Parser.DOT i);
  (";",     fun i -> Parser.SEMI i);
  (",",     fun i -> Parser.COMMA i);
  ("/",     fun i -> Parser.SLASH i);
  (":",     fun i -> Parser.COLON i);
  ("::",    fun i -> Parser.COLONCOLON i);
  ("=",     fun i -> Parser.EQ i);
  ("==",    fun i -> Parser.EQEQ i);
  ("[",     fun i -> Parser.LSQUARE i); 
  ("<",     fun i -> Parser.LT i);
  ("{",     fun i -> Parser.LCURLY i); 
  ("(",     fun i -> Parser.LPAREN i); 
  ("<-",    fun i -> Parser.LEFTARROW i); 
  ("{|",    fun i -> Parser.LCURLYBAR i); 
  ("[|",    fun i -> Parser.LSQUAREBAR i); 
  ("}",     fun i -> Parser.RCURLY i);
  (")",     fun i -> Parser.RPAREN i);
  ("]",     fun i -> Parser.RSQUARE i);
  (">",     fun i -> Parser.GT i);
  ("|}",    fun i -> Parser.BARRCURLY i);
  ("|>",    fun i -> Parser.BARGT i);
  ("|]",    fun i -> Parser.BARRSQUARE i);
  ("\n",    fun i -> Parser.NEWLINE i); 
  (";;",    fun i -> Parser.DOUBLESEMI i); 

  (* Special compound symbols: *)
  (":=",    fun i -> Parser.COLONEQ i);
  ("->",    fun i -> Parser.ARROW i);
  ("=>",    fun i -> Parser.DARROW i);
  ("==>",   fun i -> Parser.DDARROW i);
]

(* Support functions *)

type buildfun               =   info -> Parser.token
let (symbolTable :(string,buildfun) Hashtbl.t) 
                            =   Hashtbl.create 1024
let _                       =   List.iter (fun (str,f) -> Hashtbl.add symbolTable str f) reservedWords

let isInitialCapital str    = let s = String.get str 0 in s >= 'A' && s <= 'Z'  

let createID i str          =   (* info -> string -> token *)
  try (Hashtbl.find symbolTable str) i
  with _ -> 
      if isInitialCapital str 
        then Parser.UCID {i=i;v=str} 
        else Parser.LCID {i=i;v=str}

let lineno                  =   ref 1
and depth                   =   ref 0
and start                   =   ref 0
and filename                =   ref ""
and startLex                =   ref dummyinfo
let create inFile stream    =   if not (Filename.is_implicit inFile) 
                                    then filename   := inFile
                                    else filename   := Filename.concat (Sys.getcwd()) inFile;
                                lineno := 1; start := 0; Lexing.from_channel stream
let newline lexbuf          =   incr lineno; start := (Lexing.lexeme_start lexbuf)
let info    lexbuf          =   createInfo (!filename) (!lineno) (Lexing.lexeme_start lexbuf - !start)
let text                    =   Lexing.lexeme
let stringBuffer            =   ref (String.create 2048)
let stringEnd               =   ref 0
let resetStr ()             =   stringEnd := 0
let addStr ch               =
    let x                       =   !stringEnd in
    let buffer                  =   !stringBuffer in
    if x = String.length buffer 
    then begin
        let newBuffer   = String.create (x*2) in
        String.blit buffer 0 newBuffer 0 x;
        String.set newBuffer x ch;
        stringBuffer    := newBuffer;
        stringEnd       := x+1
    end 
else begin
        String.set buffer x ch;
        stringEnd := x+1
    end
let getStr ()               = String.sub (!stringBuffer) 0 (!stringEnd)

let extractLineno yytext offset =
  int_of_string (String.sub yytext offset (String.length yytext - offset))

}


(* The main body of the lexical analyzer *)

let digit       = ['0'-'9'] 
let init        = ['a'-'z' 'A'-'Z' '_' ]
let tail        = ['a'-'z' 'A'-'Z' '_' '0'-'9' '\'']
let tabs        = [' ' '\009' '\012']
let op          = ['~' '%' '\\' '+' '-' '&' '|' ':' '`' '$']
let symbol      = ['*' '#' '/' '!' '?' '^' '(' ')' '{' '}' '[' ']' '<' '>' '.' ';' '_' ',' '=' '\'']
let nl          = tabs*("\r")?"\n"
let comment     = "/*" 
let comment_end = "*/"  

rule token = parse
  tabs+                 { token lexbuf }
| nl                    { newline lexbuf; token lexbuf }
| digit+                { Parser.INTV{i=info lexbuf; v=int_of_string (text lexbuf)} }
| digit+ '.' digit+     { Parser.FLOATV{i=info lexbuf; v=float_of_string (text lexbuf)} }
| init tail*            { createID (info lexbuf) (text lexbuf) }
| ":=" | "<:" | "<-" | "->" | "=>" | "==>" | "{|" | "|}" | "<|" | "|>" 
| "[|" | "|]" | "=="    { createID (info lexbuf) (text lexbuf) }
| op+                   { createID (info lexbuf) (text lexbuf) }
| symbol                { createID (info lexbuf) (text lexbuf) }
| ";;" nl               { Parser.HOGE(info lexbuf) }
| eof                   { Parser.EOF(info lexbuf)   }
| comment_end           { error (info lexbuf) "Unmatched end of comment" } 
| comment               { depth := 1; startLex := info lexbuf; comment lexbuf; token lexbuf } 
| _                     { error (info lexbuf) "Illegal character" }

and comment = parse
  comment               { depth := succ !depth; comment lexbuf } 
| comment_end           { depth := pred !depth; if !depth > 0 then comment lexbuf } 
| eof                   { error (!startLex) "Comment not terminated" } 
| [^ '\n']              { comment lexbuf }
| "\n"                  { newline lexbuf; comment lexbuf } 

and escaped = parse
  'n'	                { '\n'      }
| 't'	                { '\t'      }
| '\\'	                { '\\'      }
| '"'                   { '\034'    }
| '\''	                { '\''      }
| digit digit digit     { let x = int_of_string(text lexbuf) in 
    if  x > 255 
        then    error (info lexbuf) "Illegal character constant"
        else    Char.chr x    
}
| [^ '"' '\\' 't' 'n' '\'']
                        { error (info lexbuf) "Illegal character constant" }

(*  *)
