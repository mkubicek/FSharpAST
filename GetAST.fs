module GetAST
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Quotations.Patterns

// method to concatenate strings
let (^) l r = sprintf "%s%s" l r

// Create a checker instance (ignore notifications)
let checker = FSharpChecker.Create()

let mutable output = ""
let mutable astOutput = ""

let ignore = "ignore"

let addToOutput str =
    output <- output + str + "\n"

let toAst str =
    astOutput <- astOutput + str

let getUntypedTree (file, input) = 
  // Get compiler options for a single script file
  let checkOptions = checker.GetProjectOptionsFromScript(file, input) |> Async.RunSynchronously
  // Run the first phase (untyped parsing) of the compiler

  let untypedRes = checker.ParseFileInProject(file, input, checkOptions) |> Async.RunSynchronously
  match untypedRes.ParseTree with
  | Some tree -> tree
  | None -> failwith "Something went wrong during parsing!"
 
/// Walk over all module or namespace declarations 
let rec visitModulesAndNamespaces modulesOrNss =
  for moduleOrNs in modulesOrNss do
    let (SynModuleOrNamespace(lid, isModule, decls, xmlDoc, attribs, synAccess, m)) = moduleOrNs
    //printfn "Namespace or module: %A" lid
    toAst "PFSSynModuleOrNamespace("
    visitDeclarations decls
    toAst ")"

/// Walk over a pattern - this is for example used in 
/// let <pat> = <expr> or in the 'match' expression
and visitPattern = function
  | SynPat.Wild(_) -> printfn "  .. underscore pattern"
  | SynPat.Named(pat, name, _, _, _) ->
      visitPattern pat
      //printfn "  .. named as '%s'" name.idText
//| SynPat.LongIdent(LongIdentWithDots(ident, _), _, _, _, _, _) ->
      //printfn "  identifier: %s" (String.concat "." [ for i in ident -> i.idText ])
  | pat -> () /// printfn " - not supported pattern: %A" pat

/// Walk over an expression
and visitExpression = function
  | SynExpr.IfThenElse(cond, trueBranch, falseBranchOpt, _, _, _, _) ->
      addToOutput "IfThenElse"
      addToOutput "("
      visitExpression trueBranch
      addToOutput ")"
      addToOutput "("
      falseBranchOpt |> Option.iter visitExpression
      addToOutput ")"
      //printfn "Conditional: %A" cond
      ///visitExpression cond
      ///visitExpression trueBranch
      ///falseBranchOpt |> Option.iter visitExpression 

  | SynExpr.LetOrUse(_, _, bindings, body, range) ->
      addToOutput "LetDeclaration"
      let mutable counter = 0

      if bindings.Length > 0 then
          addToOutput "("
      for binding in bindings do
        let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, pat, retInfo, body, m, sp)) = binding
        visitExpression body
        if counter <> 0 then
            addToOutput ","
        if bindings.Length = counter + 1 then
            addToOutput ")"
        //visitPattern pat 
        //visitExpression body
        counter <- counter + 1
      //printfn "And the following body:"
      visitExpression body
  | SynExpr.Const(con, _) -> 
      addToOutput "Const"
      // printfn "Const %A" con
  | SynExpr.Do(expr, range) -> 
      addToOutput "Const"
  | SynExpr.Paren(expr, range, range2, range3) -> 
      visitExpression expr
      addToOutput "Paren"
  | expr -> 
    addToOutput "Something unrecognized"
    () /// printfn " - not supported expression: %A" expr

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
        addToOutput "Quote"
    | SynExpr.Const(i1,i2) ->
        toAst "PFSConst"
    | SynExpr.Typed(i1,i2,i3) ->
        visitSynExpr i1
        visitSynType i2
        addToOutput "Typed"
    | SynExpr.Tuple(synExprList,i2,i3) ->
        toAst "PFSTuple"
    | SynExpr.ArrayOrList(i1,i2,i3) ->
        addToOutput "ArrayOrList"
    | SynExpr.Record(i1,i2,i3,i4) ->
        //SynExpr.Record(
        // (baseType, baseCtorArgs, mBaseCtor, sepAfterBase, mInherits) optional, 
        // (copyExpr, sepAfterCopyExpr) optional, 
        // (recordFieldName, fieldValue, sepAfterField) list, 
        // mWholeExpr
        //)
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
        addToOutput "While"
    | SynExpr.For(i1,i2,i3,i4,i5,i6,i7) ->
        toAst "PFSFor("
        visitSynExpr i6
        toAst ")"
    | SynExpr.ForEach(i1,i2,i3,i4,i5,i6,i7) ->
        toAst "PFSForEach("
        visitSynExpr i6
        toAst ")"
    | SynExpr.ArrayOrListOfSeqExpr(i1,i2,i3) ->
        toAst "PFSArrayOrListOfSeqExpr("
        visitSynExpr i2
        addToOutput ")"
    | SynExpr.CompExpr(i1,i2,i3,i4) ->
        toAst "PFSCompExpr"
    | SynExpr.Lambda(i1,i2,i3,i4,i5) ->
        addToOutput "Lambda"
    | SynExpr.MatchLambda(i1,i2,i3,i4,i5) ->
        addToOutput "MatchLambda"
    | SynExpr.Match(i1,i2,i3,i4,i5) ->
        addToOutput "Match"
    | SynExpr.Do(i1,i2) ->
        toAst "PFSDo("
        visitSynExpr i1
        toAst ")"
    | SynExpr.Assert(i1,i2) ->
        addToOutput "Assert"
    | SynExpr.App(i1,i2,synExpr1,synExpr2,i5) ->
        // 1+2 becomes App(App(+,1),2)
        toAst "PFSApp("
        visitSynExpr synExpr1
        toAst ","
        visitSynExpr synExpr2
        toAst ")"
    | SynExpr.TypeApp(i1,i2,i3,i4,i5,i6,i7) ->
        addToOutput "TypeApp"
    | SynExpr.LetOrUse(i1,i2,synBindings,i4,i5) ->
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
    | SynExpr.TryWith(i1,i2,i3,i4,i5,i6,i7) ->
        addToOutput "TryWith"
    | SynExpr.TryFinally(i1,i2,i3,i4,i5) ->
        addToOutput "TryFinally"
    | SynExpr.Lazy(i1,i2) ->
        addToOutput "Lazy"
    | SynExpr.Sequential(i1,i2,synExpr1,synExpr2,i5) ->
        toAst "PFSSequential("
        visitSynExpr synExpr1
        toAst ","
        visitSynExpr synExpr2
        toAst ")"
    | SynExpr.IfThenElse(i1,i2,i3,i4,i5,i6,i7) ->
        addToOutput "IfThenElse"
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
        addToOutput "LongIdentSet"
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
        toAst "PARSING_ERROR:_
        ARBITRARY_AFTER_ERROR"
    | expr -> ()

//and visitConst synConst = function
//    | int16(val1) -> addToOutput val1
//    | _ -> ()
    
/// Walk over a list of declarations in a module. This is anything that you can write as a top-level inside module (let bindings,nested modules, type declarations etc.)
and visitDeclarations decls =
  let mutable counter = 0
  for declaration in decls do
    match declaration with
    | SynModuleDecl.ModuleAbbrev(identifier,longidentifier,range) -> ()
    | SynModuleDecl.Let(isRec, bindings, range) ->
        let mutable letcounter = 0
        toAst "PFSLet("
        for binding in bindings do
          let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, pat (*8*), retInfo, body(*10*), m, sp)) = binding
          visitSynExpr body
          letcounter <- letcounter + 1
          if bindings.Length <> letcounter then
            toAst ","
        toAst ")"
    | SynModuleDecl.DoExpr(i1,expr,i3) -> 
        // expr is of type SynExpr.Do, wrap it into SynModuleDecl.DoExpr anyway
        toAst "PFSDoExpr("
        visitSynExpr expr
        toAst ")"
    | SynModuleDecl.Open(longIdent,range) ->
        toAst "PFSOpen"
    | SynModuleDecl.Types(synTypeDefnList,range) ->
        toAst "PFSTypes("
        for synTypeDefn in synTypeDefnList do
            let(TypeDefn(synComponentInfo, synTypeDefnRepr, _, range)) = synTypeDefn
            match synTypeDefnRepr with
            | ObjectModel(synTypeDefnKind, synMemberDefns, range) -> 
                visitMemberDefinitions synMemberDefns
            | Simple(synTypeDefnSimpleRepr, range) -> ()
        toAst ")"
    | SynModuleDecl.HashDirective(parsedHashDirective,range) ->
        toAst "PFSHashDirective()"
    | SynModuleDecl.NestedModule(_,decls,_,range) -> 
        toAst "PFSNestedModule("
        visitDeclarations decls
        toAst ")"
    | _ -> () /// printfn " - not supported declaration: %A" declaration

    counter <- counter + 1
    if decls.Length <> counter then
      toAst ","
    

and visitMemberDefinitions memDefns =
  let mutable counter = 0
  for synMemberDefn in memDefns do
    match synMemberDefn with
    | SynMemberDefn.ImplicitCtor(_,_,_,_,_) -> toAst ignore
    | SynMemberDefn.NestedType(synTypeDefn,synAccess, range) -> ()
    | SynMemberDefn.Member(synBinding, range) ->
        toAst "PFSMember("
        let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, pat, retInfo, body, m, sp)) = synBinding
        visitSynExpr body
        toAst ")"
    | SynMemberDefn.LetBindings(synBindingList, _,_, range) -> ()
    | SynMemberDefn.Open(longIdent,range) ->
        addToOutput "open"
    | SynMemberDefn.Interface(synType, synMemberDefnsInnerOpt,_)->
        toAst "PFSInterface("
        match synMemberDefnsInnerOpt with
        | Some synMemberDefnsInnerOpt -> visitMemberDefinitions synMemberDefnsInnerOpt
        | None -> ()
        toAst ")"
    | _ -> ()
    counter <- counter + 1
    if memDefns.Length <> counter then
      toAst ","


and extractImplementationFileDetails tree =
    // Extract implementation file details
    let mutable result = ""
    match tree with
    | ParsedInput.ImplFile(implFile) ->
        // Extract declarations and walk over them
        let (ParsedImplFileInput(fn, script, name, _, _, modules, _)) = implFile
        visitModulesAndNamespaces modules
        //printf "%s" output
        | _ -> failwith "F# Interface file (*.fsi) not supported."

