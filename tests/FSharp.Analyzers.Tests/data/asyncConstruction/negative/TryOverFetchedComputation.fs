module Test

open System.Collections.Generic
open System.Threading.Tasks

let private inFlight = Dictionary<string, Task<string>>()
let private fallback: Task<string> = Task.FromResult "default"

// CORRECT: the 'try' guards a lookup in a cache of work that is already running.
// The KeyNotFoundException is raised while the 'try' is still on the stack,
// so the handler does observe it, and the Task belongs to whoever started it.
let get (key: string) : Task<string> =
    try
        inFlight.[key]
    with :? KeyNotFoundException ->
        fallback

// CORRECT: a module-level value, which is a read and not a call.
let always () : Task<string> =
    try
        fallback
    with _ ->
        Task.FromResult "x"

type Holder = { Pending: Task<string> }

// CORRECT: a field read.
let pending (h: Holder) : Task<string> =
    try
        h.Pending
    with _ ->
        fallback

// CORRECT: a Task built from a value already in hand.
// 'int s' throws synchronously, and the Task it produces has finished before it is returned.
let parse (s: string) : Task<int> =
    try
        Task.FromResult(int s)
    with _ ->
        Task.FromResult -1
