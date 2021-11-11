module Wasp.Wasp.Hi (hi) where

hi :: String
hi = "Hi"

{-
Building all executables for `waspc' once. After a successful build of all of them, only specified executables will be rebuilt.
waspc> configure (lib + exe)
Configuring waspc-0.2.2.2...
waspc> build (lib + exe)
Preprocessing library for waspc-0.2.2.2..
Building library for waspc-0.2.2.2..
[1 of 2] Compiling Paths_waspc
[2 of 2] Compiling Wasp.Wasp.Hi
Preprocessing executable 'wasp' for waspc-0.2.2.2..
Building executable 'wasp' for waspc-0.2.2.2..
[1 of 2] Compiling Main
[2 of 2] Compiling Paths_waspc
Linking .stack-work/dist/x86_64-osx/Cabal-3.2.1.0/build/wasp/wasp ...
ld: can't open output file for writing: .stack-work/dist/x86_64-osx/Cabal-3.2.1.0/build/wasp/wasp, errno=21
clang: error: linker command failed with exit code 1 (use -v to see invocation)
`gcc' failed in phase `Linker'. (Exit code: 1)

--  While building package waspc-0.2.2.2 (scroll up to its section to see the error) using:
      /Users/matija/.stack/setup-exe-cache/x86_64-osx/Cabal-simple_mPHDZzAJ_3.2.1.0_ghc-8.10.7 --builddir=.stack-work/dist/x86_64-osx/Cabal-3.2.1.0 build lib:waspc exe:wasp --ghc-options " -fdiagnostics-color=always"
    Process exited with code: ExitFailure 1
-}
