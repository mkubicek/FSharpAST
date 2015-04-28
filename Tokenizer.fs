module Tokenizer
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.IO

let sourceTok = FSharpSourceTokenizer([], "C:\\test.fsx")
let tokenizer = sourceTok.CreateLineTokenizer("let answer=42")

/// Tokenize a single line of F# code
let rec tokenizeLine (tokenizer:FSharpLineTokenizer) state =
  match tokenizer.ScanToken(state) with
  | Some tok, state ->
      // Print token name
      printf "%s " tok.TokenName
      // Tokenize the rest, in the new state
      tokenizeLine tokenizer state
      | None, state -> state
      
/// tokenizeLine tokenizer 0L

let filepath = "input.fsharp"

let lines = File.ReadAllText(filepath).Split('\r','\n')
  
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