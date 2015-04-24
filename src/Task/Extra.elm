module Task.Extra where
{-| Contains a list of convenient functions that cover common use cases
for tasks.

# Chaining Tasks
@docs optional, parallel

# Communicating with Mailboxes
@docs broadcast, intercept, interceptSuccess, interceptError

# Make operations async
@docs computeLazyAsync

# Looping forever
@docs loop
-}

import Task   exposing (Task, ThreadID, fail, succeed, sleep, spawn, sequence, andThen, onError)
import Signal exposing (Address, send)
import List
import Time   exposing (Time)

{-| Analogous to `Task.sequence`.
Schedule a list of tasks to be performed in parallel as opposed to in series
as is the case with `Task.sequence`.

*Note that there is no guarantee that the tasks will be performed or complete
in the order you have stated. This is why you may use the returned `ThreadID`
for re-ordering or consider integrating a sorting mechanism within your program.*
-}
parallel : List (Task error value) -> Task error (List ThreadID)
parallel tasks =
  sequence (List.map spawn tasks)


{-| Sends a value to a list of addresses at once.
-}
broadcast : List (Address a) -> a -> Task error ()
broadcast addresses value =
  parallel ( List.map (\address -> send address value) addresses )
    `andThen` \_ -> succeed ()


{-| Similar to `Task.sequence`.
The difference with `Task.sequence` is that it doesn't return an `error` if
any individual task fails. If an error is encountered, then this function will
march on and perform the next task ignoring the error.
-}
optional : List (Task x value) -> Task y (List value)
optional list = case list of
  [] -> succeed []
  task :: tasks ->
    ( task
        `andThen` \value -> Task.map ((::) value) ( optional tasks ) )
    `onError` \_ -> optional tasks


{-| Runs a task repeatedly every given milliseconds.

    loop 1000 myTask -- Runs my task every second
-}
loop : Time -> Task error value -> Task error ()
loop every task =
  task
    `andThen` \_ -> sleep every
    `andThen` \_ -> loop every task


{-| Intercept the values computed by a task by sending them to appropriate addresses.
If the task is successful, the value will be sent to the successAddress,
if the task fails, the error value will be sent to the failAddress.
The result task will just respectively `succeed` or `fail` the computed value
such as making the interception process feel as though the task is unaffected.

    intercept successAddress failAddress myTask
-}
intercept : Address value -> Address error -> Task error value -> Task error value
intercept successAddress failAddress =
  interceptError failAddress >> interceptSuccess successAddress

{-| Intercept the successful value computed by a task by sending it to the given address.
The result task will just `succeed` after being sent to the address thus making
the interception process feel as though the task is unaffected.
-}
interceptSuccess : Address value -> Task error value -> Task error value
interceptSuccess successAddress task =
  task
    `andThen` \value  -> send successAddress value
    `andThen` \_      -> succeed value

{-| Intercept the error value computed by a task by sending it to the given address.
The result task will just `fail` after being sent to the address thus making
the interception process feel as though the task is unaffected.
-}
interceptError : Address error -> Task error value -> Task error value
interceptError failAddress task =
  task
    `onError` \error  -> send failAddress error
    `andThen` \_      -> fail error


{-| Compute a lazy value asynchronously and send the result to an address.
-}
computeLazyAsync : Address value -> (() -> value) -> Task error ()
computeLazyAsync address lazy =
  ( spawn <|
      succeed lazy
        `andThen` \f -> succeed (f ())
        `andThen` \value -> send address value )
    `andThen` \_ -> succeed ()
