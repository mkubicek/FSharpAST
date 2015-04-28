module Play

open System
open System.IO
open GetAST

let rec getAllFiles dir pattern =
    seq { yield! Directory.EnumerateFiles(dir, pattern)
          for d in Directory.EnumerateDirectories(dir) do
          yield! getAllFiles d pattern }
 
let fs = getAllFiles Environment.CurrentDirectory "*.fs"
let fsx = getAllFiles Environment.CurrentDirectory "*.fsx"

let allFSharpFilePaths = Seq.append fs fsx
let allFSharpFileContents = seq { for filePath in allFSharpFilePaths do yield File.ReadAllText(filePath)}
let allCode = String.concat "\n" allFSharpFileContents

printf "%s" allCode
File.WriteAllText("out.txt", allCode)
