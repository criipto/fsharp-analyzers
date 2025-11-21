module Program

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open Argu
open System
open System.IO

type Arguments =
  | [<MainCommand; ExactlyOnce>] File of path: string
  | [<AltCommandLine("-s")>] Syntax
  | [<AltCommandLine("-t")>] Types
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | File _ -> "specify the F# file to parse."
      | Syntax -> "print syntax tree."
      | Types -> "print type declarations."

[<EntryPoint>]
let main args =
  let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
  let argParser = ArgumentParser.Create<Arguments>(programName = "fsharp-tree-viewer", errorHandler=errorHandler)
  let parsedArgs = argParser.Parse args
  let filename = parsedArgs.GetResult File
  let printSyntaxArg = parsedArgs.TryGetResult Syntax |> Option.isSome
  let printTypesArg = parsedArgs.TryGetResult Types |> Option.isSome
  // Print both if none specified
  let printSyntax = printSyntaxArg || not printTypesArg
  let printTypes = printTypesArg || not printSyntaxArg
  let program = filename |> File.ReadAllText

  let checker = FSharpChecker.Create(keepAssemblyContents=true)
  let projOptions, _ =
      checker.GetProjectOptionsFromScript(filename, SourceText.ofString program, assumeDotNetFramework=false)
      |> Async.RunSynchronously
  let parseResults, checkFileAnswer =
    checker.ParseAndCheckFileInProject(filename, 0, SourceText.ofString program, projOptions)
    |> Async.RunSynchronously

  if printSyntax then
    Console.WriteLine "====== Untyped syntax tree ======"
    Console.WriteLine   parseResults.ParseTree

  if printTypes then
    match checkFileAnswer with
    | FSharpCheckFileAnswer.Succeeded res ->
        match res.ImplementationFile with
        | Some i ->
          Console.WriteLine "====== Typed declarations ======"
          for d in i.Declarations do
            Console.WriteLine d
        | None ->
          Console.WriteLine "File did not typecheck"
    | res ->
      Console.WriteLine $"Parsing did not finish... %A{res}"

  0