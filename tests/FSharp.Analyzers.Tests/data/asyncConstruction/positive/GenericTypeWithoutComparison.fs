module Test

open System.Threading.Tasks

// This generic type is untranslatable for FSharp.Compiler.Service,
// which must not stop this finding from being reported.
// See https://github.com/dotnet/fsharp/issues/19118.
type Container<'a> = Wrapped of 'a

let private fetch () : Task<string> = Task.FromResult "ok"

// BUG: a non-CE function returns the try/with value directly.
// The caller awaits it, so the handler guards construction only.
let run () : Task<string> =
    try
        fetch ()
    with _ -> Task.FromResult "fallback"
