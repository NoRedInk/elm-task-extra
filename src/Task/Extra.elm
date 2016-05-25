module Task.Extra exposing (optional, parallel, delay, loop, performFailproof)
{-| Contains a list of convenient functions that cover common use cases
for tasks.

# Chaining Tasks
@docs optional, parallel

# Delay a task
@docs delay

# Looping forever
@docs loop

# Commands
@docs performFailproof
-}

import Task    exposing (Task, fail, succeed, sequence, andThen, onError)
import Process exposing (sleep, spawn)
import List
import Time   exposing (Time)

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
optional list = case list of
  [] -> succeed []
  task :: tasks ->
    ( task `andThen` \value -> Task.map ((::) value) ( optional tasks ) )
      `onError` \_ -> optional tasks


{-| Runs a task repeatedly every given milliseconds.

    loop 1000 myTask -- Runs `myTask` every second
-}
loop : Time -> Task error value -> Task error ()
loop every task =
  task
    `andThen` \_ -> sleep every
    `andThen` \_ -> loop every task

{-| Delay a task by a given amount of time in milliseconds.
-}
delay : Time -> Task error value -> Task error value
delay time task =
  sleep time
    `andThen` \_ -> task

{-| Command the runtime system to perform a task that is guaranteed to
not fail. The most important argument is the
[`Task`](http://package.elm-lang.org/packages/elm-lang/core/latest/Task#Task)
which describes what you want to happen. But you also need to provide
a function to tag the success outcome, so as to have a message to feed
back into your application. Unlike with the standard
[`perform`](http://package.elm-lang.org/packages/elm-lang/core/latest/Task#perform),
you need not provide a function to tag a failing outcome, because the
[`Never`](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#Never)
in the type `Task Never a` expresses that no possibly failing task is
allowed in that place anyway.

A typical use of the function is `Date.now |> performFailproof CurrentDate`.
-}
performFailproof : (a -> msg) -> Task Never a -> Cmd msg
performFailproof =
    Task.perform never

-- from http://package.elm-lang.org/packages/elm-community/basics-extra:
never : Never -> a
never n =
    never n
