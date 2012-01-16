module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit

import Control.Monad (when)
import qualified Data.Set as Set
import System.Exit (exitFailure)

import Bustle.Types
import Bustle.Renderer

main = defaultMain tests
  where
    tests = [ testGroup "Disconnections don't affect participants"
                [ testCase "One participant, no disconnection" test_participants
                , testCase "One participant, which disconnects" test_participants_with_disconnect
                ]
            ]

-- Tests that services visible in a log are listed as participants even if they
-- disconnect from the bus before the end of the log. This is a regression test
-- for a bug I almost introduced.
activeService = UniqueName ":1.1"
swaddle = map (\m -> DetailedMessage 0 m Nothing)
sessionLog =
    [ Connected activeService
    , Signal (U activeService) Nothing $ Member "/" Nothing "Hello"
    ]
sessionLogWithDisconnect = sessionLog ++ [ Disconnected activeService ]
expectedParticipants = [ (activeService, Set.empty) ]

test_ l expected = expected @=? ps
  where
    rr = process (swaddle l) []
    ps = sessionParticipants (rrApplications rr)

test_participants = test_ sessionLog expectedParticipants
test_participants_with_disconnect = test_ sessionLogWithDisconnect expectedParticipants
