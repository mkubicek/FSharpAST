# FSharpASTExtractor
Tool to extract textual encoded AST from F# code 

Depends on a MacOSX Xamarin installation including F# compiler:

### MacOSX installation notes:###
 * Install the Xamarin IDE http://xamarin.com/download
 * Open the project

### ..on Linux? some installation hints:###

Get and make Mono MRE work
 * http://www.mono-project.com/download/

Get and make work F# Compiler
 * Use branch fsharp_30 from https://github.com/fsharp/fsharp/tree/master
 * ./autogen.sh --prefix=/Library/Frameworks/Mono.framework/Versions/Current/
 * make
 * sudo make install
 * Run tests in tests/fsharp/core/: run-all.sh
	
Get and make work the FSharp.Compiler.Service
 * https://github.com/fsharp/FSharp.Compiler.Service


### Usage:###

####Simple####
```
FSharpASTExtractor.exe simple input.fs out.txt
```
Parse F# code from the file `input.fs` and output a textually represented AST in `out.txt`.


####Sample input:#####
```
let rec fib n = 
  if (n=1||n=2) then
   1
  else 
   let result = fib(n-1) 
              + fib(n-2)
   result
         
for i in 1 .. 10 do 
 printfn "%d:%d" i (fib i) 
```

#####Sample output:#####
```
PFSSynModuleOrNamespace(PFSLet(PFSIfThenElse(PFSParen(PFSConst),PFSParen(PFSLetOrUse(PFSApp(PFSApp(PFSIdent(operator),PFSApp(PFSIdent(fib),PFSParen(PFSApp(PFSApp(PFSIdent(operator),PFSIdent(n)),PFSConst)))),PFSApp(PFSIdent(fib),PFSParen(PFSApp(PFSApp(PFSIdent(operator),PFSIdent(n)),PFSConst))));PFSIdent(result))))),PFSDoExpr(PFSForEach(PFSApp(PFSApp(PFSApp(PFSIdent(printfn),PFSConst),PFSIdent(i)),PFSParen(PFSApp(PFSIdent(fib),PFSIdent(i))))))) 
```

####Advanced####
```
FSharpASTExtractor.exe extendedLet inputPath out.txt
```
Extract just largest consecutive let bindings to the file `out.txt`. Complex F# code input can be split into manageable pieces. Input paths resolving to a directory will be scanned recursively for .fs and .fsx F# code files and their parsing results will be accumulated in the `out.txt`.

#####Sample output:#####
```
File: #(./fsharp3sample-25592/SampleProviders/Samples.Hadoop/Helpers.fs)#
Code: #(let theProxyProcessAgent = startProxyProcessAgent())#
AST: #(PFSLet(PFSApp(PFSIdent(startProxyProcessAgent),PFSConst)))#
ModuleLevel: #(True)#
...
```
