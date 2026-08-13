import CodeBlock from '@theme/CodeBlock'

#### Custom Server URL

One app serves both your pages and your API, so your pages look for the API on their own origin. There is nothing to point anywhere, not even when you use a [custom domain](#custom-domain).

The `--custom-server-url` option is left over from when the client and the server were deployed as two separate apps. It no longer has any effect, so you can drop it from your deployment scripts:

<CodeBlock language="shell">{
`# The option is ignored:
wasp deploy ${props.provider} ${props.command}${props.example ? ` ${props.example}` : ''} --custom-server-url https://api.myapp.com`
}</CodeBlock>

If you really do serve your API from another origin, that's the `REACT_APP_API_URL` [client environment variable](#client-environment-variables), which is baked into your pages when they are built.
