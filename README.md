# FSharpAST
Matching output of the F# parser with an independent F# parser implemented in Smalltalk, PetitParser (http://smalltalkhub.com/#!/~MilanKubicek/FSharpGrammar)

Depends on MacOSX Xamarin installation with working F# mono installation.

###Some MacOSX installation notes:###
 * Get FSharp working http://fsharp.org/use/mac/
 * Get the Xamarin IDE http://xamarin.com/download
 * Make FSharp working in Xamarin http://developer.xamarin.com/guides/cross-platform/fsharp/fsharp_support_overview/
 * ~~Get the F# compiler services nuget http://fsharp.github.io/FSharp.Compiler.Service/~~ already included in project

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
