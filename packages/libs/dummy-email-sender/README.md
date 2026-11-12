# @wasp/lib-dummy-email-sender

The `Dummy` email sender and its dev mailbox. Instead of sending emails, it logs them and keeps them in `.wasp/mailbox/`, with an inbox page and an HTTP API for tests and agents.

The generated server mounts the routes at `/__wasp/mailbox` in development. See the "Dev mailbox" section of the Wasp email docs.
