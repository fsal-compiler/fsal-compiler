module Fs.AL.SampleProject.CodeunitTest04SingleInstanceModule

open Fs.AL.Core
open Fs.AL.Core.ALComplexValues
open Fs.AL.Core.Abstract
open Fs.AL.Packages
open Fs.AL.SampleProject.RecordTest01

//#nowarn "20"


[<ALSingleInstanceCodeunit(60004)>]
module SingleInstanceModule =
    let add2 x = x + 2
