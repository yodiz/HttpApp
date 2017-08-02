// include Fake lib
#r @"packages\FAKE\tools\FakeLib.dll"
open Fake


let buildDir  = @".\build\"
let testDir   = @".\test\"
let deployDir = @".\deploy\"
let packagesDir = @".\packages"


Target "Clean" (fun _ ->
    CleanDirs [buildDir; testDir; deployDir]
)

Target "CompileApp" (fun _ ->
    !! @"src\*.fsproj"
      |> MSBuildRelease buildDir "Build"
      |> Log "AppBuild-Output: "
)

// Default target
Target "Default" (fun _ ->
    trace "Hello World from FAKE"
)


"Clean"
  ==> "CompileApp"


// start build
RunTargetOrDefault "CompileApp"