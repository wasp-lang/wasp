# Waspls

This directory contains source code of the `wasp` language server (aka `waspls`)
and this README is aimed at the contributors to the project.

## Overview

`waspls` is implemented in Haskell. It depends on `waspc` for parsing and
analyzing wasp source code. Cabal is currently configured to look in `../waspc`
for the `waspc` package, so do not move `waspls` out of the `wasp` repo without
adjusting this line in `cabal.project`.

## Usage

Use in any place an LSP server can be used. Run `waspls --help` for usage
information.
