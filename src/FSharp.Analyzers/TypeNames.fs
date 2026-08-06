module Idura.FSharp.Analyzers.TypeNames

open FSharp.Compiler.Symbols

// FUTURE: In version 10.0.300 of FSharp.Compiler.Services, the BasicQualifiedName becomes an
// option, which should allow the "try ... with" below to be dropped in favour of it.
/// The CLR-style qualified name the compiler identifies the type by.
/// None for a type that has none, such as a tuple, an anonymous record or a type parameter.
let basicQualifiedName (t: FSharpType) =
    try
        Some t.BasicQualifiedName
    with :? System.InvalidOperationException ->
        None

/// The type as the user would write it. The empty display context has nothing opened, so this is
/// fully qualified, but it is F# syntax rather than a CLR name: 'Pool<string>', not 'Pool`1'.
let displayName (t: FSharpType) = t.Format FSharpDisplayContext.Empty
