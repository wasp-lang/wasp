import CodeBlock from '@theme/CodeBlock'

#### Custom Server URL

In the [split deployment mode](../../intro.md#deployment-modes), the client is built with the server's URL baked in. If you want it to connect to a different server URL (for example, a custom domain for your server), use the `--custom-server-url` option:

<CodeBlock language="shell">{
`wasp deploy ${props.provider} ${props.command}${props.example ? ` ${props.example}` : ''} --custom-server-url https://api.myapp.com`
}</CodeBlock>

In the default single deployment mode the client talks to its own origin, so the option has no effect and Wasp prints a notice if you pass it.
