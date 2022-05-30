{-

(Task a b) is what we put into the queue instead of simply (a) when the actions' results are desired. A Task contains the (a) value as well as an additional mutable variable of type (b) into which the action's result will be placed. The type of the mutable variable is either MVar or TVar, depending on whether we want access to it via IO or STM.

-}
module Unfork.Async.WithResult.Task
    (
        Task (..),
    )
    where

data Task a b =
    Task
        { arg :: !a
        , resultVar :: !b
        }
