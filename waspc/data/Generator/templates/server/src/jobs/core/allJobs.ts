{{={= =}=}}

// This module imports all jobs and is imported by the server to ensure
// any schedules that are not referenced are still loaded by NodeJS.

{=# jobs =}
import '../{= name =}.js'
{=/ jobs =}
