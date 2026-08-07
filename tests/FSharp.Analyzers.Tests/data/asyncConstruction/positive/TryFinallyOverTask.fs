module Test

open System.Threading
open System.Threading.Tasks

// BUG: The semaphore is released when the Task is constructed and returned,
// so the work runs outside the lock it was meant to hold.
let withSemaphore (sem: SemaphoreSlim) (work: unit -> Task<string>) =
    try
        sem.Wait()
        work ()
    finally
        sem.Release() |> ignore
