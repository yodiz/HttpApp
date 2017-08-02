// include Fake lib
#r "System.Xml.Linq"
#r @"packages\FAKE\tools\FakeLib.dll"
open Fake
open System.Xml.Linq

RestorePackages()


let buildDir  = @".\build\"
let artifactsNuGetDir = @"./artifacts/nuget/"


Target "Clean" (fun _ ->
    CleanDirs [buildDir; artifactsNuGetDir]
)

Target "Build" (fun _ ->
    !! @"src\*.fsproj"
      |> MSBuildRelease buildDir "Build"
      |> Log "AppBuild-Output: "
)

Target "BuildNuGet" (fun _ ->
    let doc = System.Xml.Linq.XDocument.Load("./HttpApp.nuspec")
    let vers = doc.Descendants(XName.Get("version", doc.Root.Name.NamespaceName)) 

    NuGet (fun p -> 
    {p with
        Version = (Seq.head vers).Value
        OutputPath = artifactsNuGetDir
        WorkingDir = buildDir
        })  "./HttpApp.nuspec"
)

// Default target
Target "Default" (fun _ ->
    trace "Hello World from FAKE"
)


"Clean"
  ==> "Build"
  ==> "BuildNuGet"
  


// start build
RunTargetOrDefault "BuildNuGet"