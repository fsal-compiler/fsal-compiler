
// to load everything in the project including references
#load "./imports.fsx"

open Fs.AL.SampleProject.Demo01HTTPRequests

let translationManagement = TranslationManagement()

translationManagement.ProcedureForTesting "testing"
    