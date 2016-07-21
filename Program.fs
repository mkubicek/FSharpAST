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

let extractSingle (filepath, outfile) =
    let input = File.ReadAllText(filepath)
     
    let tree = GetAST.getUntypedTree(filepath, "/home/user/dummy.fsx", input) 
    let test = GetAST.traverseTree tree

    File.WriteAllText(outfile, GetAST.astOutput)

let extractLetModules filePath =
    GetAST.reset ()

    let input = File.ReadAllText(filePath)
    let test = GetAST.traverseTree (GetAST.getUntypedTree(filePath, "/home/user/dummy.fsx", input))

    for letDecl in GetAST.LetDeclarations do
        if (IsSubLet letDecl AllTopLevelLets) = false then
            AllTopLevelLets <- letDecl :: AllTopLevelLets
    ()

let extractAllLetDecl (inputpath, outfile) =
    if Directory.Exists (inputpath) then
        let fs = getAllFilePaths inputpath "*.fs"
        let fsx = getAllFilePaths inputpath "*.fsx"
        let allFSharpFilePaths = Seq.append fs fsx
        for filePath in allFSharpFilePaths do extractLetModules (filePath)
    else extractLetModules (inputpath)

    let mutable outputString = ""

    for topLevelLet in (List.sortBy (fun (elem : LetDeclaration) -> elem.SourceCode.Length) AllTopLevelLets) do
        outputString <- outputString + "File: #(" + topLevelLet.FileName + ")#" + Environment.NewLine
        outputString <- outputString + "Code: #(" + topLevelLet.SourceCode + ")#" + Environment.NewLine
        outputString <- outputString + "AST: #(" + topLevelLet.ASTRep + ")#" + Environment.NewLine
        outputString <- outputString + "ModuleLevel: #(" + topLevelLet.IsModuleLevel.ToString() + ")#" + Environment.NewLine
        outputString <- outputString + Environment.NewLine

    File.WriteAllText(outfile, outputString)
    ()

[<EntryPoint>]
let main argv = 
    let errorMessage = "Called with wrong parameters. Sample usage: 'FSharpASTExtractor.exe simple <inputFile> <outputFile>' or 'FSharpASTExtractor.exe extended <inputDirectoryOrFile> <outputFile>'" 

    if argv.Length < 3 then 
        printfn "%s" errorMessage
    else
        match argv.[0], argv.[1], argv.[2]  with
        | "extendedLet", path, outFile -> extractAllLetDecl (path, outFile)
        | "simple", filepath, outFile -> extractSingle (filepath, outFile)
        | _ -> printfn "%s" errorMessage
    
    0