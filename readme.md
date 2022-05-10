# F# to AL compiler

## Building the project 

NOTE: Projects with large packages generated from Type Provider have been commented out as compiling them takes a very long time, the output have been stored in pre-compiled nuget packages in ./nupkg, for this reason i suggest using the pre-compiled nuget packages directly.

#### Dependencies: 

- .NET 6.0 SDK (https://dotnet.microsoft.com/en-us/download/dotnet/thank-you/sdk-6.0.300-windows-x64-installer)


#### Making local nuget packages accessible   

- open the nuget config file (`%APPDATA%\NuGet\NuGet.Config`)
- add the following entry to packageSources:
- `<add key="fsalcompiler" value="C:\<path/to/repository>\Fs.AL\nupkg" />`

#### .. 