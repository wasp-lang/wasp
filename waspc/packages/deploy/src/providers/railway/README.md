More of a TODO list probably, to note down few things that are known "gotchas" during implementation.
To revisit when PR is created.

1. `railway login` does not work in non-interactive mode. Current impl asks for options to login or not atm, so just remove it if there is no way around it and force user to login outside of the one-liner.
2. Revisit "global options", like --wasp-exe <path> stuff
