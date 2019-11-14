open Format
open Core
open Support.Pervasive
open Support.Error
open Syntax
open Arg 
open Type

exception NoRuleApplies

let rec isnumericval ctx = function 
    | TmZero(_)         -> true
    | TmSucc(_,t1)      -> isnumericval ctx t1
    | _                 -> false

let rec isval ctx = function 
    | TmAbs(_,_,_,_)                -> true
    | TmTrue(_)                     -> true
    | TmFalse(_)                    -> true
    | t when isnumericval ctx t     -> true
    | _                             -> false

let rec eval1 ctx = function 
    | TmApp(fi,TmAbs(_,x,tyT11,t12),v2) when isval ctx v2 
                                        ->  termSubstTop v2 t12 
    | TmApp(fi,v1,t2) when isval ctx v1 ->  TmApp(fi,v1,eval1 ctx t2) 
    | TmApp(fi,t1,t2)                   ->  TmApp(fi,eval1 ctx t1,t2) 
    | TmIf(_,TmTrue(_),t2,t3)           ->  t2
    | TmIf(_,TmFalse(_),t2,t3)          ->  t3
    | TmIf(fi,t1,t2,t3)                 ->  let t1' = eval1 ctx t1 in TmIf(fi, t1', t2, t3)
    | TmSucc(fi,t1)                     ->  let t1' = eval1 ctx t1 in TmSucc(fi, t1')
    | TmPred(_,TmZero(_))               ->  TmZero(dummyinfo)
    | TmPred(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) 
                                        ->  nv1
    | TmPred(fi,t1)                     ->  TmPred(fi, eval1 ctx t1)
    | TmIsZero(_,TmZero(_))             ->  TmTrue(dummyinfo)
    | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) 
                                        ->  TmFalse(dummyinfo)
    | TmIsZero(fi,t1)                   ->  let t1' = eval1 ctx t1 in TmIsZero(fi, t1')
    | _                                 ->  raise NoRuleApplies

let rec eval ctx t =
    try eval ctx (eval1 ctx t) 
    with NoRuleApplies -> t



let rec process_command ctx = function 
    | Eval(fi,t)                ->  
            let tyT = typeof ctx t in 
            printtm_ATerm true ctx (eval ctx t); 
            print_break 1 2; pr ": "; printty tyT; force_newline(); ctx
    | Bind(fi,x,bind)           ->  pr ("Now, "^x^ " is a variable: "); prbindingty ctx bind; force_newline(); addbinding ctx x bind 


let print_eval ctx cmd      = 
    open_hvbox 0; 
    process_command ctx cmd; 
    print_flush ()




