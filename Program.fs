open System
open System.IO
open GetAST
open Microsoft.FSharp.Compiler.SourceCodeServices

let rec getAllFilePaths dir pattern =
    seq { yield! Directory.EnumerateFiles(dir, pattern)
          for d in Directory.EnumerateDirectories(dir) do
          yield! getAllFilePaths d pattern }

let IsSubLet (letDecl : LetDeclaration) (list : List<LetDeclaration>) = 
    List.exists (fun (elem : LetDeclaration) -> (elem.FileName = letDecl.FileName) 
                                                && (elem.StartLine <= letDecl.StartLine) 
                                                && (elem.EndLine >= letDecl.EndLine)
                                                && (elem.StartColumn <= letDecl.StartColumn)
                                                && (elem.EndColumn >= letDecl.EndColumn)) list

let mutable AllTopLevelLets : List<LetDeclaration> = List.empty<LetDeclaration>

let visitAST () =
    let filepath = "input.fsharp"
    let input = File.ReadAllText(filepath)

    // Get the AST of sample F# code
    let tree = GetAST.getUntypedTree(filepath,"/home/user/dummy.fsx", input) 
    let test = GetAST.traverseTree tree

    printf "%s" GetAST.astOutput

let extractLetModules filePath =
    GetAST.reset ()

    let input = File.ReadAllText(filePath)
    let test = GetAST.traverseTree (GetAST.getUntypedTree(filePath, filePath, input))

    for letDecl in GetAST.LetDeclarations do
        if (IsSubLet letDecl AllTopLevelLets) = false then
            AllTopLevelLets <- letDecl :: AllTopLevelLets
    ()

let extractFromAll () =
    let fs = getAllFilePaths "/Users/mkubicek/Dropbox/Uni/FSharp/FSharpDemoCode" "*.fs"
    let fsx = getAllFilePaths "/Users/mkubicek/Dropbox/Uni/FSharp/FSharpDemoCode" "*.fsx"
    let allFSharpFilePaths = Seq.append fs fsx
    for filePath in allFSharpFilePaths do extractLetModules(filePath)

    let mutable outputString = ""

    for topLevelLet in (List.sortBy (fun (elem : LetDeclaration) -> elem.SourceCode.Length) AllTopLevelLets) do
        outputString <- outputString + "File: #(" + topLevelLet.FileName + ")#" + Environment.NewLine
        outputString <- outputString + "Code: #(" + topLevelLet.SourceCode + ")#" + Environment.NewLine
        outputString <- outputString + "AST: #(" + topLevelLet.ASTRep + ")#" + Environment.NewLine
        outputString <- outputString + "ModuleLevel: #(" + topLevelLet.IsModuleLevel.ToString() + ")#" + Environment.NewLine
        outputString <- outputString + Environment.NewLine

    File.WriteAllText("out.txt", outputString)
    ()

[<EntryPoint>]
let main argv = 
    visitAST ()
    //extractFromAll()
    0 // return an integer exit code









