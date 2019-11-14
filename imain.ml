open Format
open Core
open Support.Pervasive
open Support.Error
open Syntax
open Arg 
open Type
open Eval

let interpreter lexbuf  = Parser.input      Lexer.token lexbuf 
let compiler    lexbuf  = Parser.toplevel   Lexer.token lexbuf

let parse' machine in_channel =   (* machine -> in_channel -> command list *) 
    let lexbuf              =   Lexing.from_channel in_channel  in
    let result,ctx          =   try     machine lexbuf emptycontext 
                                with  | Parsing.Parse_error -> error (Lexer.info lexbuf) "Parse error" 
                                      | e -> raise e 
    in
    Parsing.clear_parser(); close_in in_channel; result

(* #####################
 * #### INTERPRETER ####
 * ##################### *)

let process' ctx ()         = 
    let cmds                = parse' interpreter stdin in 
    List.iter (print_eval ctx) cmds

(* interpreter *) 
let rec parse_error s = print_endline s; flush stdout ;; 
let main' () =
    while true do 
        try process' emptycontext (); print_endline "debug loop"
        with    End_of_file -> print_endline "end_of_file"
            |   e           -> raise e
    done

let _ = Printexc.catch (fun () -> try main' (); 0 with Exit x -> x) () 


