To run the deploy package as a standalone TS project, run:
```sh
npm install
npm run build
npm run cli fly ...
```

You need to supply `--wasp-exe` and `--wasp-project-dir` arguments to the `npm run cli` command, as those when used within Wasp are supplied directly from Haskell, so it might seem invisible. But for manual trigger of this CLI and local testing, those arguments are base arguments that required for this CLI to work.
