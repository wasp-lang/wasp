module Tests.WaspCompletionContainerTest (waspCompletionContainerTest) where

import ContainerTest (ContainerTest, makeContainerTest)
import ShellCommands (waspCliCompletion)

waspCompletionContainerTest :: ContainerTest
waspCompletionContainerTest =
  makeContainerTest
    "wasp-completion"
    [ 
      -- use dev version of wasp cli if possible
      waspCliCompletion
      -- todo: how to test completion?
    ]

