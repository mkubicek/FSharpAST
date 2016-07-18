# FSharpAST
Extract textual encoded AST from F# code to learn more about F# AST structure and validate independent F# parser implementations like http://smalltalkhub.com/#!/~MilanKubicek/FSharpGrammar

Depends on a MacOSX Xamarin installation including F# compiler:

### MacOSX installation notes:###
 * Make FSharp working http://fsharp.org/use/mac/
 * Install the Xamarin IDE http://xamarin.com/download
 * Make FSharp working in Xamarin http://developer.xamarin.com/guides/cross-platform/fsharp/fsharp_support_overview/

###..on Linux? some installation hints:###

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

---

Brew and automake on Mac OS X
 * ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
 * brew doctor
 * brew install automake
