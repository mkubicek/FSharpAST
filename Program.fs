open System
open System.IO
open GetAST
open Tokenizer
open Microsoft.FSharp.Compiler.SourceCodeServices

let visitAST () =
    let filepath = "input.fsharp"
    let input = File.ReadAllText(filepath)

    // File name in Unix format
    let file = "/home/user/dummy.fsx"
    // Get the AST of sample F# code
    let tree = GetAST.getUntypedTree(file, input) 
    let test = GetAST.extractImplementationFileDetails tree

    printf "%s" GetAST.output

let tokenize () = 
    //let tokenizer = sourceTok.CreateLineTokenizer("let answer=42")

    /// Tokenize a single line of F# code
    let rec tokenizeLine (tokenizer:FSharpLineTokenizer) state =
      match tokenizer.ScanToken(state) with
      | Some tok, state ->
          // Print token name
          printf "%s " tok.TokenName
          // Tokenize the rest, in the new state
          tokenizeLine tokenizer state
      | None, state -> state

    let lines = """
      let x = 
         let y = 2
         x + y """.Split('\r','\n')

    /// Print token names for multiple lines of code
    let rec tokenizeLines state count lines = 
      match lines with
      | line::lines ->
          // Create tokenizer & tokenize single line
          printfn "\nLine %d" count
          let tokenizer = sourceTok.CreateLineTokenizer(line)
          let state = tokenizeLine tokenizer state
          // Tokenize the rest using new state
          tokenizeLines state (count+1) lines
      | [] -> ()

    lines
    |> List.ofSeq
    |> tokenizeLines 0L 1

[<EntryPoint>]
let main argv = 
    visitAST ()
   
    0 // return an integer exit code

