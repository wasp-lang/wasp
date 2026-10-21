import CodeBlock from '@theme/CodeBlock'

#### Custom Server URL

If you want your client to connect to a different server URL (for example, if you're using a custom domain for your server), use the `--custom-server-url` option:

<CodeBlock language="shell">{
`wasp deploy ${props.provider} ${props.command}${props.example ? ` ${props.example}` : ''} --custom-server-url https://api.myapp.com`
}</CodeBlock>
