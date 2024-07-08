:::caution LSP Problems

If you are using TypeScript, your editor may sometimes report type and import errors even while `wasp start` is running.

This happens when the TypeScript Language Server gets out of sync with the current code.
If you're using VS Code, you can manually restart the language server by opening the command palette and selecting _"TypeScript: Restart TS Server."_
Open the command pallete with:
 - `Ctrl` + `Shift` + `P` if you're on Windows or Linux.
 - `Cmd` + `Shift` + `P` if you're on a Mac.

:::
