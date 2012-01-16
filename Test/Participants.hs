-- Tests that services visible in a log are listed as participants even if they
-- disconnect from the bus before the end of the log. This is a regression test
-- for a bug I almost introduced.
module Main where

import Control.Monad (when)
import qualified Data.Set as Set
import System.Exit (exitFailure)

import Bustle.Types
import Bustle.Renderer

activeService = UniqueName ":1.1"
swaddle = map (\m -> DetailedMessage 0 m Nothing)
sessionLog =
    [ Connected activeService
    , Signal (U activeService) Nothing $ Member "/" Nothing "Hello"
    ]
sessionLogWithDisconnect = sessionLog ++ [ Disconnected activeService ]
expectedParticipants = [ (activeService, Set.empty) ]

assertEquals expected actual =
    when (expected /= actual) $ do
        putStrLn "Expected:"
        print expected
        putStrLn "Got:"
        print actual
        exitFailure

test l expected = do
    let rr = process (swaddle l) []
        ps = sessionParticipants (rrApplications rr)

    assertEquals expected ps

main = do
    test sessionLog expectedParticipants
    test sessionLogWithDisconnect expectedParticipants
