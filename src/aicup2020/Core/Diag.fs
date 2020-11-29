namespace Jaina.Core

module Diag =
    let elapsed msg f = 
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        let returnValue = f()
#if DEBUG
        printfn $"{msg} elapsed Time: {timer.ElapsedMilliseconds} ms"  
#endif
        returnValue

    let elapsedRelease msg f = 
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        let returnValue = f()
        printfn $"{msg} elapsed Time: {timer.ElapsedMilliseconds} ms"
        returnValue

