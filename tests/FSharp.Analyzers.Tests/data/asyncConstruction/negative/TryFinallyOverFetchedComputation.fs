module Test

open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

let private inFlight = Dictionary<string, Task<string>>()

// CORRECT: the read lock guards the lookup, which is the only thing inside the 'try' that needs it,
// so releasing it when the already-running Task is handed back is right.
let getOrStart (rwLock: ReaderWriterLockSlim) (key: string) : Task<string> =
    rwLock.EnterReadLock()

    try
        inFlight.[key]
    finally
        rwLock.ExitReadLock()

// CORRECT: the same, reading out of a mutable cell rather than a dictionary.
// Compare positive/TryFinallyOverTask, which is this shape with a call that starts the work in the tail
// instead of a read
let current (sem: SemaphoreSlim) (holder: Task<string> ref) : Task<string> =
    try
        sem.Wait()
        holder.Value
    finally
        sem.Release() |> ignore
