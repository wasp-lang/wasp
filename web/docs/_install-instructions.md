import CodeBlock from '@theme/CodeBlock'
import Trackable from '@site/src/components/Trackable'

To install the latest Wasp version, open your terminal and run:

<Trackable event="Install Command: Copy">

```sh
npm i -g @wasp/wasp-cli@latest
```

</Trackable>

You can install Wasp {props.version} specifically by passing the version to the install script:

<Trackable event="Install Command: Copy">

<CodeBlock language="sh">{
`npm i -g @wasp/wasp-cli@${props.version}`
}</CodeBlock>

</Trackable>
