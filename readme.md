# F# to AL compiler

| ![image](https://user-images.githubusercontent.com/36763595/178295900-438adbb5-feed-40c4-ae93-6f424dfe87e1.png) | ![image](https://user-images.githubusercontent.com/36763595/178296039-8d98d95d-6c54-49df-ba51-7ce05119d098.png) |
|:---:|:---:|

# NOTE: 
this is the source code for the thesis [F\# type provider and compiler for the AL programming language ](https://digikogu.taltech.ee/en/item/d0c6a5dc-90e7-4add-b34c-0ed9dfff1bb4) as it was when it was written. 

this project is a proof of concept and not usable in production.

## Building the project 

NOTE: Projects with large packages generated from Type Provider have been commented out as compiling them takes a very long time, the output have been stored in pre-compiled nuget packages in ./nupkg, for this reason i suggest using the pre-compiled nuget packages directly.

#### Dependencies: 

- .NET 6.0 SDK (https://dotnet.microsoft.com/en-us/download)


#### Making local nuget packages accessible   

- open the nuget config file (`%APPDATA%\NuGet\NuGet.Config`)
- add the following entry to packageSources:
- `<add key="fsalcompiler" value="C:\<path/to/repository>\nupkg" />`

#### Building the project and compiling

- restore the dependencies using `dotnet restore`
- build the solution using `dotnet build`
- open the directory `src\Fs.AL.SampleProject`
- inside the directory is an `fsal.json` file, which specifies the paths used for resolving dependencies by the compiler, ensure that there is a reference to the correct .NET installation e.g. `"C:\\Program Files\\dotnet\\packs\\Microsoft.NETCore.App.Ref\\6.0.6\\ref\\net6.0"` and a reference to the design-time DLLs of any used type provider (e.g. the Fable.JsonProvider dll `"C:\\users\\username\\.nuget\\packages\\fable.jsonprovider\\1.0.1\\lib\\netstandard2.0\\Fable.JsonProvider.dll"`)
- after that run `dotnet fsi compile.fsx`, which runs the compiler in the current working directory

