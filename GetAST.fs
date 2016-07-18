module GetAST
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Quotations.Patterns

type LetDeclaration = struct
   val SourceCode : string (* holds the piece of code *)
   val ASTRep : string (* represents the AST of the source code *)
   val FileName : string
   val StartColumn : int
   val EndColumn : int
   val StartLine : int
   val EndLine : int
   val IsModuleLevel : bool

   new (sourceCode, astRep, fileName, startColumn, endColumn, startLine, endLine, isModuleLevel) =
      {SourceCode = sourceCode; ASTRep = astRep; FileName = fileName; StartColumn = startColumn; EndColumn = endColumn; StartLine = startLine; EndLine = endLine; IsModuleLevel = isModuleLevel}
end

let mutable LetDeclarations = List.empty<LetDeclaration>

// method to concatenate strings
let (^) l r = sprintf "%s%s" l r

// Create a checker instance (ignore notifications)
let checker = FSharpChecker.Create()

let mutable output = ""

// holds the filename of the current source code file
let mutable Filename = ""

// holds the current source code file
let mutable InputSourceLines = [| "" |]

// holds the ast represantation of a whole source file
let mutable astOutput = ""

// holds the ast representation of the current let declaration
let mutable currentLet = ""

let ignore = "ignore"

let GetStringFromLines (lines:string[], startLine:int, endLine:int, startColumn:int, endColumn:int) =
    let mutable output = [| |]
    for i = 0 to lines.Length - 1 do
        if i >=
         (startLine - 1)
         then
            if i = (startLine - 1) then
                output <- Array.append output [| lines.[i].Substring(startColumn) |]
            else if i = (endLine - 1) then
                output <- Array.append output [| lines.[i].Substring(0, endColumn) |]
            else if i < (endLine - 1) then
                output <- Array.append output [| lines.[i] |] 
            else 
                ()
    output

let addToOutput str =
    output <- output + str + "\n"

let reset () =
    output <- ""
    astOutput <- ""
    LetDeclarations <- List.empty<LetDeclaration>

let toAst str =
    astOutput <- astOutput + str
    currentLet <- currentLet + str


let getUntypedTree (filePath, filePathForParser, input) = 
  InputSourceLines <- File.ReadAllLines(filePath)
  Filename <- filePath
  // Get compiler options for a single script file
  let checkOptions = checker.GetProjectOptionsFromScript((*file*) "/dev/null", input) |> Async.RunSynchronously
  // Run the first phase (untyped parsing) of the compiler
 
  let untypedRes = checker.ParseFileInProject(filePathForParser, input, checkOptions) |> Async.RunSynchronously
  match untypedRes.ParseTree with
  | Some tree -> tree
  | None -> failwith "Something went wrong during parsing!"
 
/// Walk over all module or namespace declarations 
let rec visitModulesAndNamespaces modulesOrNss =
  for moduleOrNs in modulesOrNss do
    let (SynModuleOrNamespace(lid, isModule, decls, xmlDoc, attribs, synAccess, m)) = moduleOrNs
    //printfn "Namespace or module: %A" lid
    toAst "PFSSynModuleOrNamespace("; visitDeclarations decls; toAst ")"

and visitSynType synType =
    match synType with
    | _ -> ()
and visitSynExpr synExpr = 
    addToOutput ("type" ^ synExpr.GetType().Name)
    match synExpr with
    | SynExpr.Paren(synExpr1,i2,i3,i4) ->
        toAst "PFSParen("
        visitSynExpr synExpr1
        toAst ")"
    | SynExpr.Quote(i1,i2,i3,i4,i5) ->
        toAst "TODO:Quote"
    | SynExpr.Const(i1,i2) ->
        toAst "PFSConst"
    | SynExpr.Typed(i1,i2,i3) ->
        toAst "TODO:Typed"
        visitSynExpr i1
        visitSynType i2
        addToOutput "Typed"
    | SynExpr.Tuple(synExprList,i2,i3) ->
        toAst "PFSTuple"
    | SynExpr.ArrayOrList(i1,i2,i3) ->
        toAst "ArrayOrList"
    | SynExpr.Record(i1,i2,i3,i4) ->
        toAst "TODO:Record"
        match i1 with
        | Some i1-> 
            match i1 with
            | (synType1,synExpr1,x3,x4,x5) -> visitSynExpr synExpr1
        | None -> ()
        match i2 with
        | Some i2-> 
            match i2 with
            | (synExpr2,blockSep) -> visitSynExpr synExpr2
        | None -> ()
        // { f1=e1; ...; fn=en }
        for ((recordFieldName, rangeList), synExpr3, blockSeperator) in i3 do
            match synExpr3 with
            | Some synExpr3 -> visitSynExpr synExpr3
            | None -> ()
        addToOutput "Record"
    | SynExpr.New(i1,i2,i3,i4) ->
        toAst "PFSNew("
        visitSynExpr i3
        toAst ")"
    | SynExpr.ObjExpr(i1,i2,i3,i4,i5,i6) ->
        toAst "PFSObjExpr("
        toAst ")"
    | SynExpr.While(i1,i2,i3,i4) ->
        toAst "PFSWhile("
        visitSynExpr i3
        toAst ")"
    | SynExpr.For(i1,i2,i3,i4,i5,i6,i7) ->
        toAst "PFSFor("
        visitSynExpr i6
        toAst ")"
    | SynExpr.ForEach(i1,i2,i3,i4,i5,i6,i7) ->
        toAst "PFSForEach("
        visitSynExpr i6
        toAst ")"
    | SynExpr.ArrayOrListOfSeqExpr(i1,i2,i3) ->
        toAst "PFSArrayOrListOfSeqExpr" // i2 just holds a CompExpr. Not of interest.
    | SynExpr.CompExpr(i1,i2,i3,i4) ->
        toAst "PFSCompExpr"
    | SynExpr.Lambda(i1,i2,i3,i4,i5) ->
        toAst "PFSLambda("
        visitSynExpr i4
        toAst ")"
    | SynExpr.MatchLambda(i1,i2,i3,i4,i5) ->
        addToOutput "MatchLambda"
    | SynExpr.Match(i1,i2,i3,i4,i5) ->
        toAst "PFSMatch("
        let mutable counter = 0
        for synMatchClause in i3 do
            let (Clause(SynPat, SynExprOpt, SynExpr, range,SequencePointInfoForTarget)) = synMatchClause
            visitSynExpr SynExpr
            counter <- counter + 1
            if i3.Length <> counter then
              toAst ","
        toAst ")"
    | SynExpr.Do(i1,i2) ->
        toAst "PFSDo("
        visitSynExpr i1
        toAst ")"
    | SynExpr.Assert(i1,i2) ->
        toAst "PFSAssert"
    | SynExpr.App(i1,i2,synExpr1,synExpr2,i5) ->

        // 1+2 becomes App(App(+,1),2)
        toAst "PFSApp("
        visitSynExpr synExpr1
        toAst ","
        visitSynExpr synExpr2
        toAst ")"
    | SynExpr.TypeApp(i1,i2,i3,i4,i5,i6,i7) ->
        addToOutput "TypeApp"

    | SynExpr.LetOrUse(i1,i2,synBindings,i4, i5) ->
        let lengthAstOutputBefore = astOutput.Length
        toAst "PFSLetOrUse("
        let mutable counter = 0
        for binding in synBindings do
            let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, pat, retInfo, body, m, sp)) = binding
            visitSynExpr body
            counter <- counter + 1
            if synBindings.Length <> counter then
              toAst ","
        toAst ";"
        visitSynExpr i4
        toAst ")"

        let extractedLines = GetStringFromLines (InputSourceLines, i5.StartLine, i5.EndLine, i5.StartColumn, i5.EndColumn)
        let myLet = new LetDeclaration((extractedLines |> String.concat System.Environment.NewLine), astOutput.Substring(lengthAstOutputBefore), Filename, i5.StartColumn, i5.EndColumn, i5.StartLine, i5.EndLine, false)
        LetDeclarations <- myLet :: LetDeclarations

    | SynExpr.TryWith(i1,i2,i3,i4,i5,i6,i7) ->
        toAst "TryWith"
    | SynExpr.TryFinally(i1,i2,i3,i4,i5) ->
        toAst "TryFinally"
    | SynExpr.Lazy(i1,i2) ->
        addToOutput "Lazy"
    | SynExpr.Sequential(i1,i2,synExpr1,synExpr2,i5) ->
        toAst "PFSSequential("
        visitSynExpr synExpr1
        toAst ","
        visitSynExpr synExpr2
        toAst ")"
    | SynExpr.IfThenElse(i1,ifBranch,elseBranchOpt,i4,i5,i6,i7) ->
        toAst "PFSIfThenElse("
        visitSynExpr ifBranch
        elseBranchOpt |> Option.iter (toAst ","; visitSynExpr)
        toAst ")"
    | SynExpr.Ident(i1) ->
        toAst "PFSIdent("
        let name = i1.ToString() in
        if name.Contains("op") then
            toAst "operator"
        else
            toAst (i1.ToString())
        toAst ")"
    | SynExpr.LongIdent(i1,longIdentWithDots,i3,i4) ->
        toAst "PFSLongIdent("
        let (LongIdentWithDots(a1,a2)) = longIdentWithDots
        System.String.Join(".",a1) |> toAst
        toAst ")"
    | SynExpr.LongIdentSet(i1,i2,i3) ->
        toAst "PFSLongIdentSet"
    | SynExpr.DotGet(i1,i2,i3,i4) ->
        addToOutput "DotGet"
    | SynExpr.DotIndexedGet(i1,i2,i3,i4) ->
        addToOutput "DotIndexedGet"
    | SynExpr.DotIndexedSet(i1,i2,i3,i4,i5,i6) ->
        addToOutput "DotIndexedSet"
    | SynExpr.NamedIndexedPropertySet(i1,i2,i3,i4) ->
        addToOutput "NamedIndexedPropertySet"
    | SynExpr.DotNamedIndexedPropertySet(i1,i2,i3,i4,i5) ->
        addToOutput "DotNamedIndexedPropertySet"
    | SynExpr.TypeTest(i1,i2,i3) ->
        addToOutput "TypeTest"
    | SynExpr.Upcast(i1,i2,i3) ->
        addToOutput "Upcast"
    | SynExpr.Downcast(i1,i2,i3) ->
        addToOutput "Downcast"
    | SynExpr.InferredUpcast(i1,i2) ->
        addToOutput "InferredUpcast"
    | SynExpr.InferredDowncast(i1,i2) ->
        addToOutput "InferredDowncast"
    | SynExpr.Null(i1) ->
        addToOutput "Null"
    | SynExpr.AddressOf(i1,i2,i3,i4) ->
        addToOutput "AddressOf"
    | SynExpr.TraitCall(i1,i2,i3,i4) ->
        addToOutput "TraitCall"
    | SynExpr.JoinIn(i1,i2,i3,i4) ->
        addToOutput "JoinIn"
    | SynExpr.ImplicitZero(i1) ->
        addToOutput "ImplicitZero"
    | SynExpr.YieldOrReturn(i1,i2,i3) ->
        addToOutput "YieldOrReturn"
    | SynExpr.LetOrUseBang(i1,i2,i3,i4,i5,i6,i7) ->
        addToOutput "LetOrUseBang"
    | SynExpr.DoBang(i1,i2) ->
        addToOutput "DoBang"
    | SynExpr.ArbitraryAfterError(i1,i2) ->
        toAst "PARSING_ERROR:ARBITRARY_AFTER_ERROR"
    | SynExpr.FromParseError(i1,i2) ->
        toAst "PARSING ERROR"
    | expr -> 
        toAst "UNKNOWN"
        ()
    
/// Walk over a list of declarations in a module. This is anything that you can write as a top-level inside module (let bindings,nested modules, type declarations etc.)
and visitDeclarations decls =
  let mutable counter = 0
  for declaration in decls do
    match declaration with
    | SynModuleDecl.ModuleAbbrev(identifier,longidentifier,range) -> ()
    | SynModuleDecl.Let(isRec, bindings, range) -> (* module level let *)
        let lengthAstOutputBefore = astOutput.Length
        let mutable letcounter = 0
        toAst "PFSLet("
        for binding in bindings do
          let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, pat (*8*), retInfo, body(*10*), m, sp)) = binding
          visitSynExpr body
          letcounter <- letcounter + 1
          if bindings.Length <> letcounter then
            toAst ","
        toAst ")"

        let extractedLines = GetStringFromLines (InputSourceLines, range.StartLine, range.EndLine, range.StartColumn, range.EndColumn)
        let myLet = new LetDeclaration((extractedLines |> String.concat System.Environment.NewLine), astOutput.Substring(lengthAstOutputBefore), Filename, range.StartColumn, range.EndColumn, range.StartLine, range.EndLine, true)
        LetDeclarations <- myLet :: LetDeclarations

    | SynModuleDecl.Open(longIdent,range) ->
        toAst "PFSOpen"
    | SynModuleDecl.Types(synTypeDefnList,range) ->
        toAst "PFSTypes"
        for synTypeDefn in synTypeDefnList do
            let(TypeDefn(synComponentInfo, synTypeDefnRepr, _, range)) = synTypeDefn
            match synTypeDefnRepr with
            | ObjectModel(synTypeDefnKind, synMemberDefns, range) -> 
                toAst "("
                visitMemberDefinitions synMemberDefns
                toAst ")"
            | Simple(synTypeDefnSimpleRepr, range) -> ()
        
    | SynModuleDecl.HashDirective(parsedHashDirective,range) ->
        toAst "PFSHashDirective"
    | SynModuleDecl.NestedModule(_,decls,_,range) -> 
        toAst "PFSNestedModule("
        visitDeclarations decls
        toAst ")"
    | SynModuleDecl.DoExpr(i1,i2,i3) -> 
        // i2 is expr of type SynExpr.Do -> We get something like PFSDoExpr(PFSDo(..))
        toAst "PFSDoExpr("
        visitSynExpr i2
        toAst ")"
    | _ -> () /// printfn " - not supported declaration: %A" declaration

    counter <- counter + 1
    if decls.Length <> counter then
      toAst ","
    

and visitMemberDefinitions memDefns =
  let mutable counter = 0
  let mutable skip = false
  for synMemberDefn in memDefns do
    match synMemberDefn with
    | SynMemberDefn.ImplicitCtor(i1,i2,i3,i4,i5) -> 
        //toAst ignore
        skip <- true
        ()
    | SynMemberDefn.ImplicitInherit(i1,i2,i3,i4) -> 
        toAst "SynMemberDefn("
        visitSynExpr i2
        toAst ")"
    | SynMemberDefn.Member(synBinding, range) ->
        toAst "PFSMember"
        toAst "("
        let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, pat, retInfo, body, m, sp)) = synBinding
        visitSynExpr body
        toAst ")"
    | SynMemberDefn.LetBindings(synBindings, _,_, range) -> 
        toAst "PFSLetBindings"
    | SynMemberDefn.Open(longIdent,range) ->
        addToOutput "open"
    | SynMemberDefn.Interface(synType, synMemberDefnsInnerOpt,_)->
        toAst "PFSInterface("
        match synMemberDefnsInnerOpt with
        | Some synMemberDefnsInnerOpt -> visitMemberDefinitions synMemberDefnsInnerOpt
        | None -> ()
        toAst ")"
    | _ -> 
        toAst "SynMemberDefn something else"

    counter <- counter + 1
    if ((not skip) && memDefns.Length <> counter) then
      toAst ","
    skip <- false

and traverseTree tree =
     match tree with
     | ParsedInput.ImplFile(implFile) ->
        // Extract declarations and walk over them
        let (ParsedImplFileInput(fn, script, name, _, _, modules, _)) = implFile
        visitModulesAndNamespaces modules
        //printf "%s" output  
     | _ -> failwith "F# Interface file (*.fsi) not supported."