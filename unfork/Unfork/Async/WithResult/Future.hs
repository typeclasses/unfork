module Unfork.Async.WithResult.Future where

import qualified Control.Concurrent.MVar as MVar

{- |

    The result of an action unforked by 'Unfork.unforkAsyncIO'

    At first the result will be unavailable, during which time 'await' will block and 'poll' will return 'Nothing'. When the action completes, 'await' will return its result and 'poll' will return 'Just'.

-}

data Future result =
    Future
        (MVar.MVar result)

{- |

    Block until an action completes

-}

await :: Future result -> IO result
await (Future v) = MVar.readMVar v

{- |

    Returns 'Just' an action's result, or 'Nothing' if the
    action is not yet complete

-}

poll :: Future result -> IO (Maybe result)
poll (Future v) = MVar.tryReadMVar v
