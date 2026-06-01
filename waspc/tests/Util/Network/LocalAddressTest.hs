module Util.Network.LocalAddressTest where

import Test.Hspec
import Wasp.Util.Network.LocalAddress (isIPv4Address, toNipIoHostname)

spec_isIPv4Address :: Spec
spec_isIPv4Address =
  describe "isIPv4Address" $ do
    it "accepts a standard private IPv4 address" $
      isIPv4Address "192.168.1.39" `shouldBe` True

    it "accepts the boundary values 0.0.0.0 and 255.255.255.255" $ do
      isIPv4Address "0.0.0.0" `shouldBe` True
      isIPv4Address "255.255.255.255" `shouldBe` True

    it "rejects octets above 255" $
      isIPv4Address "256.0.0.1" `shouldBe` False

    it "rejects non-numeric octets" $
      isIPv4Address "192.168.1.abc" `shouldBe` False

    it "rejects addresses with the wrong number of octets" $ do
      isIPv4Address "192.168.1" `shouldBe` False
      isIPv4Address "192.168.1.1.1" `shouldBe` False

    it "rejects empty octets" $
      isIPv4Address "192..1.1" `shouldBe` False

    it "rejects hostnames" $
      isIPv4Address "my-machine.local" `shouldBe` False

spec_toNipIoHostname :: Spec
spec_toNipIoHostname =
  describe "toNipIoHostname" $ do
    it "appends .nip.io to a bare IPv4 address" $
      toNipIoHostname "192.168.1.39" `shouldBe` "192.168.1.39.nip.io"

    it "leaves non-IPv4 hostnames unchanged" $ do
      toNipIoHostname "my-machine.local" `shouldBe` "my-machine.local"
      toNipIoHostname "example.com" `shouldBe` "example.com"

    it "leaves an already-suffixed nip.io hostname unchanged" $
      toNipIoHostname "192.168.1.39.nip.io" `shouldBe` "192.168.1.39.nip.io"
