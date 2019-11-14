open Format
open Core
open Support.Pervasive
open Support.Error
open Syntax
open Arg 


val typeof          : context -> term -> ty
val prbindingty     : context -> binding -> unit 
