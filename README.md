# FSharpAST
Matching output of the F# parser with an independent F# parser implemented in Smalltalk, PetitParser (http://smalltalkhub.com/#!/~MilanKubicek/FSharpGrammar)

Depends on MacOSX Xamarin installation with working F# mono installation.

Installation on MacOSX: http://fsharp.org/use/mac/



Installation on Linux:
--Brew and automake on Mac OS X
	ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
	brew doctor
	brew install automake

Install Mono MRE
	http://www.mono-project.com/download/

Install F# Compiler
	Use branch fsharp_30 from https://github.com/fsharp/fsharp/tree/master
	./autogen.sh --prefix=/Library/Frameworks/Mono.framework/Versions/Current/
	make
	sudo make install
Run tests in tests/fsharp/core/
	run-all.sh
	
Compile FSharp.Compiler.Service
Running Mono (MRE) http://www.mono-project.com/download/
