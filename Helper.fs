open System

module Helper

let GetStringFromLines lines startLine endLine startColumn endColumn =
    let mutable output = ""
    for i = 0 to lines.Length do
        if i > startLine then
            if i+1 = startLine then
                output <- output + Substring(startColumn, lines[i])
            else
                output <- output + lines[i]

    output