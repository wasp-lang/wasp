{{={= =}=}}

// This module imports all jobs, registering them with pg-boss. The server
// imports it at startup, so that jobs not referenced anywhere else (e.g.,
// scheduled ones) still run.

{=# jobs =}
import '../{= name =}.js'
{=/ jobs =}
