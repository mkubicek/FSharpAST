module GetAST
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Quotations.Patterns

// method to concatenate strings
let (^) l r = sprintf "%s%s" l r

// Create a checker instance (ignore notifications)
let checker = FSharpChecker.Create()

let mutable output = ""

let addToOutput str =
    output <- output + str + "\n"

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
    visitDeclarations decls

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



and visitSynExpr synExpr = 
    addToOutput ("type" ^ synExpr.GetType().Name)
    match synExpr with
    | SynExpr.Paren(synExpr1,i2,i3,i4) ->
        visitSynExpr synExpr1 // ?
        addToOutput "Paren"
    | SynExpr.Quote(i1,i2,i3,i4,i5) ->
        addToOutput "Quote"
    | SynExpr.Const(i1,i2) ->
        addToOutput ("Const" ^ i1.ToString())
    | SynExpr.Typed(i1,i2,i3) ->
        addToOutput "Typed"
    | SynExpr.Tuple(i1,i2,i3) ->
        addToOutput "Paren"
    | SynExpr.ArrayOrList(i1,i2,i3) ->
        addToOutput "ArrayOrList"
    | SynExpr.Record(i1,i2,i3,i4) ->
        addToOutput "Record"
    | SynExpr.New(i1,i2,i3,i4) ->
        addToOutput "New"
    | SynExpr.ObjExpr(i1,i2,i3,i4,i5,i6) ->
        addToOutput "ObjExpr"
    | SynExpr.While(i1,i2,i3,i4) ->
        addToOutput "While"
    | SynExpr.For(i1,i2,i3,i4,i5,i6,i7) ->
        addToOutput "For"
    | SynExpr.ForEach(i1,i2,i3,i4,i5,i6,i7) ->
        addToOutput "ForEach"
    | SynExpr.ArrayOrListOfSeqExpr(i1,i2,i3) ->
        addToOutput "ArrayOrListOfSeqExpr"
    | SynExpr.CompExpr(i1,i2,i3,i4) ->
        addToOutput "CompExpr"
    | SynExpr.Lambda(i1,i2,i3,i4,i5) ->
        addToOutput "Lambda"
    | SynExpr.MatchLambda(i1,i2,i3,i4,i5) ->
        addToOutput "MatchLambda"
    | SynExpr.Match(i1,i2,i3,i4,i5) ->
        addToOutput "Match"
    | SynExpr.Do(i1,i2) ->
        addToOutput "Do"
    | SynExpr.Assert(i1,i2) ->
        addToOutput "Assert"
    | SynExpr.App(i1,i2,synExpr1,synExpr2,i5) ->
        // 1+2 becomes App(App(+,1),2)
        addToOutput "App"
        visitSynExpr synExpr1 // left side
        visitSynExpr synExpr2 // ??
    | SynExpr.TypeApp(i1,i2,i3,i4,i5,i6,i7) ->
        addToOutput "TypeApp"
    | SynExpr.LetOrUse(i1,i2,i3,i4,i5) ->
        addToOutput "LetOrUse"
    | SynExpr.TryWith(i1,i2,i3,i4,i5,i6,i7) ->
        addToOutput "TryWith"
    | SynExpr.TryFinally(i1,i2,i3,i4,i5) ->
        addToOutput "TryFinally"
    | SynExpr.Lazy(i1,i2) ->
        addToOutput "Lazy"
    | SynExpr.Sequential(i1,i2,synExpr1,synExpr2,i5) ->
        addToOutput "Sequential"
    | SynExpr.IfThenElse(i1,i2,i3,i4,i5,i6,i7) ->
        addToOutput "IfThenElse"
    | SynExpr.Ident(i1) ->
        addToOutput "Ident"
    | SynExpr.LongIdent(i1,i2,i3,i4) ->
        addToOutput "LongIdent"
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
    | expr -> ()

//and visitConst synConst = function
//    | int16(val1) -> addToOutput val1
//    | _ -> ()
    
/// Walk over a list of declarations in a module. This is anything that you can write as a top-level inside module (let bindings,nested modules, type declarations etc.)
and visitDeclarations decls =
  for declaration in decls do
    match declaration with
    | SynModuleDecl.ModuleAbbrev(_,_,_) -> ()
    | SynModuleDecl.Let(isRec, bindings, range) ->
        for binding in bindings do
          let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, pat (*8*), retInfo, body(*10*), m, sp)) = binding
          //printfn "LetDeclaration"
          //result <- result + "LetDeclaration"
          //printfn "blublubub"
          //visitPattern pat 
          // visitExpression body
          visitSynExpr body

    | SynModuleDecl.DoExpr(_,expr,_) -> visitExpression expr
    | _ -> () /// printfn " - not supported declaration: %A" declaration

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
