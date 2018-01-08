{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit

import Control.Monad (when)
import Control.Monad.State
import qualified Data.Set as Set
import Data.Monoid
import Data.List
import System.Exit (exitFailure)
import DBus (objectPath_, busName_, ReceivedMessage(ReceivedMethodReturn), firstSerial, methodReturn)

import Bustle.Types
import Bustle.Renderer

main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "Disconnections don't affect participants"
                [ testCase "One participant, no disconnection" test_participants
                , testCase "One participant, which disconnects" test_participants_with_disconnect
                ]
            , testGroup "Incremential rendering matches all-at-once rendering"
                [ testCase "rrCentreOffset" $ test_incremental_simple rrCentreOffset
                , testCase "rrTopOffset" $ test_incremental_simple rrTopOffset
                , testCase "rrShapes" $ test_incremental_list rrShapes
                , testCase "rrRegions" $ test_incremental_list rrRegions
                , testCase "rrApplications" $ test_incremental_simple rrApplications
                , testCase "rrWarnings" $ test_incremental_simple rrWarnings
                ]
            ]

-- Tests that services visible in a log are listed as participants even if they
-- disconnect from the bus before the end of the log. This is a regression test
-- for a bug I almost introduced.
activeService = UniqueName ":1.1"
dummyReceivedMessage = ReceivedMethodReturn firstSerial (methodReturn firstSerial)
swaddle messages timestamps = map (\(e, ts) -> Detailed ts e 0 dummyReceivedMessage)
                                  (zip messages timestamps)
sessionLogWithoutDisconnect =
    [ NOCEvent $ Connected activeService
    , MessageEvent $ Signal (U activeService) Nothing $ Member (objectPath_ "/") Nothing "Hello"
    ]
sessionLogWithDisconnect = sessionLogWithoutDisconnect ++ [ NOCEvent $ Disconnected activeService ]
expectedParticipants = [ (activeService, Set.empty) ]

test_ l expected = expected @=? ps
  where
    rr = process (swaddle l [1..]) []
    ps = sessionParticipants (rrApplications rr)

test_participants = test_ sessionLogWithoutDisconnect expectedParticipants
test_participants_with_disconnect = test_ sessionLogWithDisconnect expectedParticipants

-- Test that incremental rendering matches all-at-once rendering
u1 = UniqueName ":1.1"
u2 = UniqueName ":2.2"

-- This is enough names that the log needs to be rejustified to the top
os = map (OtherName . busName_ . ("Foo." ++) . (:"potato")) ['a'..'z']

m = Member "/" Nothing "Hi"

bareLog = [ NOCEvent $ Connected u1
          , MessageEvent $ Signal (U u1) Nothing m
          , NOCEvent $ Connected u2
          ]
          ++ map (\o -> NOCEvent (NameChanged o (Claimed u2))) os ++
          [ MessageEvent $ MethodCall 0 (U u1) (O (head os)) m ]

sessionLog = swaddle bareLog [1,3..]
systemLog  = swaddle bareLog [2,4..]

test_incremental_simple :: (Show b, Eq b)
                        => (RendererResult Participants -> b)
                        -> Assertion
test_incremental_simple f =
    test_incremental $ \full incremental -> f full @=? f incremental

test_incremental_list :: (Show b, Eq b)
                      => (RendererResult Participants -> [b])
                      -> Assertion
test_incremental_list f =
    test_incremental $ \fullRR incrementalRR -> do
        let full = f fullRR
            incr = f incrementalRR

        -- Compare each element in turn
        mapM_ (uncurry (@=?)) $ zip full incr
        when (length full /= length incr) $
            full @=? incr

test_incremental :: (  RendererResult Participants
                    -> RendererResult Participants
                    -> Assertion
                    )
                 -> Assertion
test_incremental f = f fullRR incrementalRR

-- TODO: it should be possible to make this work for side-by-side logs too.
-- Currently it doesn't seem to...
fullRR, incrementalRR :: RendererResult Participants
fullRR = process sessionLog []
incrementalRR = mconcat rrs
  where
    processOne m = state $ processSome [m] []
    (rrs, _) = runState (mapM processOne sessionLog) rendererStateNew
