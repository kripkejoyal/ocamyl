open Format
open Core
open Support.Pervasive
open Support.Error
open Syntax
open Arg 

exception NoRuleApplies


(* ----------- TYPING --------------- *) 

let rec typeof ctx   t      = pr "TYPEOF: ";printtm ctx t;print_newline ();  match t with
    | TmVar(fi,i,_)             -> getTypeFromContext fi ctx i 
    | TmAbs(fi,x,tyT1,t2)       -> 
            let ctx'    = addbinding ctx x (VarBind(tyT1)) in 
            let tyT2    = typeof ctx' t2 in
            TyArr(tyT1,tyT2) 
    | TmApp(fi,t1,t2)           -> 
            let tyT1 = typeof ctx t1 in 
            let tyT2 = typeof ctx t2 in 
            (match tyT1 with 
                | TyArr(tyT11,tyT12)    -> if (=) tyT2 tyT11 then tyT12 else error fi "type mismatch" 
                | _                     -> error fi "arrow type expected" )
    | TmTrue(fi)                -> TyBool
    | TmFalse(fi)               -> TyBool
    | TmZero(fi)                -> TyNat
    | TmSucc(fi,t)              -> if (=) (typeof ctx t) TyNat then TyNat else error fi "succ expects 𝐍"  
    | TmPred(fi,t)              -> if (=) (typeof ctx t) TyNat then TyNat else error fi "succ expects 𝐍"  
    | TmIsZero(fi,t)            -> if (=) (typeof ctx t) TyNat then TyBool else error fi "iszero expects 𝐍"
    | TmIf(fi,t1,t2,t3)         -> if (=) (typeof ctx t1) TyBool then 
                let tyT2 = typeof ctx t2 in
                if (=) tyT2 (typeof ctx t3) then tyT2 else error fi "resulting type of if statement mismatch" 
                else error fi "if-condition expects a boolean" 


(* ---- *) 

let prbindingty ctx = function
    | NameBind                  -> ()
    | VarBind(tyT)              -> pr ": "; printty tyT 




