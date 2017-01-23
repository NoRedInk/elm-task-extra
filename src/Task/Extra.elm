module Task.Extra exposing (optional, parallel, delay, loop)

{-| Contains a list of convenient functions that cover common use cases
for tasks.

# Chaining Tasks
@docs optional, parallel

# Delay a task
@docs delay

# Looping forever
@docs loop
-}

import Task exposing (Task, fail, succeed, sequence, andThen, onError)
import Process exposing (sleep, spawn)
import List
import Time exposing (Time)


{-| Analogous to `Task.sequence`.
Schedule a list of tasks to be performed in parallel as opposed to in series
as is the case with `Task.sequence`.

*Note that there is no guarantee that the tasks will be performed or complete
in the order you have stated. This is why you may use the returned `Process.Id`
for re-ordering or consider integrating a sorting mechanism within your program.*
-}
parallel : List (Task error value) -> Task error (List Process.Id)
parallel tasks =
    sequence (List.map spawn tasks)


{-| Similar to `Task.sequence`.
The difference with `Task.sequence` is that it doesn't return an `error` if
any individual task fails. If an error is encountered, then this function will
march on and perform the next task ignoring the error.
-}
optional : List (Task x value) -> Task y (List value)
optional list =
    case list of
        [] ->
            succeed []

        task :: tasks ->
            task
                |> andThen (\value -> Task.map ((::) value) (optional tasks))
                |> onError (\_ -> optional tasks)


{-| Runs a task repeatedly every given milliseconds.

    loop 1000 myTask -- Runs `myTask` every second
-}
loop : Time -> Task error value -> Task error ()
loop every task =
    task
        |> andThen
            (\_ ->
                sleep every
                    |> andThen (\_ -> loop every task)
            )


{-| Delay a task by a given amount of time in milliseconds.
-}
delay : Time -> Task error value -> Task error value
delay time task =
    sleep time
        |> andThen (\_ -> task)
