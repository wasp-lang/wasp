import CodeBlock from '@theme/CodeBlock'

To install the latest version of Wasp, open your terminal and run:

<Tabs groupId="wasp-installation-method">
<TabItem value="npm" label="npm">

```sh
npm i -g @wasp.sh/wasp-cli@latest
```

</TabItem>
<TabItem value="installer" label="Installer">

```sh
curl -sSL https://get.wasp.sh/installer.sh | sh
```

</TabItem>
</Tabs>

If you want to install Wasp {props.version} specifically, you can pass a version argument to the install script:

<Tabs groupId="wasp-installation-method">
<TabItem value="npm" label="npm">

<CodeBlock language="sh">{
`npm i -g @wasp.sh/wasp-cli@${props.version}`
}</CodeBlock>

</TabItem>
<TabItem value="installer" label="Installer">

<CodeBlock language="sh">{
`curl -sSL https://get.wasp.sh/installer.sh | sh -s -- -v ${props.version}`
}</CodeBlock>

</TabItem>
</Tabs>
