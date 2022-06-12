namespace Fs.AL.Compiler.CompilerServices

type FSharpToALCompilerSettings = {
    /// relative path to F# project directory
    /// e.g. "./fsharpproject", defaults to "./fsharp"
    sourcePath : string option
    /// relative path to AL project directory
    /// e.g. "./alproject", defaults to "./al"
    outputPath : string option
    /// relative or full path to search external dlls from
    /// e.g. [ "fsharp/bin/Debug/net6.0-windows" ] 
    includePaths : string list option
    /// include specific dlls in the compilation.
    /// useful for adding design-time type-providers
    /// e.g [ "%USERPROFILE%\\.nuget\\packages\\fable.jsonprovider\\1.0.1\\lib\\netstandard2.0\\Fable.JsonProvider.dll" ]
    includeDlls : string list option
    /// set if you only want to compile the files specified
    /// relative to sourcePath
    /// e.g. [ "Codeunit1.fs" ; "Codeunit2.fs" ]
    includeFiles : string list option
}
