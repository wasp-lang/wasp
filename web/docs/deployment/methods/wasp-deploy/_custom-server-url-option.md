#### Custom Server URL

If you want your client to connect to a different server URL (for example, if you're using a custom domain for your server), use the `--custom-server-url` option:

<WaspCommand npm={`npm run wasp -- deploy ${props.provider} ${props.command}${props.example ? ` ${props.example}` : ''} --custom-server-url https://api.myapp.com`} global={`wasp deploy ${props.provider} ${props.command}${props.example ? ` ${props.example}` : ''} --custom-server-url https://api.myapp.com`} />
