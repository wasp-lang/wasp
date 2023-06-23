# waspls Architecture

waspls uses the [lsp](https://hackage.haskell.org/package/lsp) library for
interacting with the Language Server Protocol.

The main entry point is `serve` in `Wasp/LSP/Server.hs`. This function sets up
the LSP server, the reactor thread, and starts everything.

The handlers to LSP notifications and requests are defined in `Wasp/LSP/Handlers.hs`
and imported by `Wasp/LSP/Server.hs` so that the `lsp` package can be told about
them. Mostly, these handlers are small functions to call out to the actual
implementation in another source file.

There are two types of handlers in waspls:

1. "Analysis handlers" that extract some syntactic or semantic information from
   source code and store it in the server state.
2. "Request handlers" that use the information stored in the server state to
   provide LSP features, such as autocompletion and goto definition.

Request handlers have read-only access to the server state, while analysis
handlers can write to the state.

## Multithreading

By default, the `lsp` package is single-threaded. Because waspls sometimes
needs to do time-intensive work, such as getting the list of exported symbols
from TypeScript files, there is a mechanism for running code on a separate
thread. The purpose of this is to avoid blocking the main thread for long periods
of time, which would make the language server feel unresponsive in the editor.

This mechanism is the "reactor thread," which continuously looks for new actions
to run on a separate thread. For details, see the documentation in `Wasp/LSP/Reactor.hs`.

# Testing Development Versions of waspls

Set the wasp executable path in the Wasp VSCode extension to `wasp-cli`. To
build the LSP, run `cabal install`. Then, restart the language server in VSCode
by running the `Wasp: Restart Wasp LSP Server` command. By default, you can
press <kbd>Ctrl+Shift+P</kbd> to open the command palette to search for the
command.

Note that, after changing the executable path in the extension settings, you
have to reload the VSCode window. You can do this either by closing and reopening
the window or by running the command `Developer: Reload Window`.
