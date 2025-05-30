# Writing Documentation

To write docs, you can use Markdown or MDX. The docs are located in the `docs` directory.

## Organization

To keep Wasp's documentation organized and consistent, follow the guidelines set
out in this document when updating or writing new documentation.

When adding a new page to the docs, read the guidelines below to determine the
best place for the page to go.

If a new section is created, add it to the list below and include some information
on how to decide if a page belongs in that section or not.

### Getting Started

Answers: What is Wasp and how do I get it?

### Essentials

Answers: Now that I have Wasp, what can I do with it?

Explains the workflow of using Wasp and the most important parts:

- How do I make a new project
- What's in the project
- How do I add more pages to the website
- How do I store stuff in a database

How do we know if a feature belongs here?

- A feature belongs here when it's a _core feature_: nearly every Wasp project is
  going to use this feature in some capacity.

### Data Model

Answers: How do persist data in Wasp?

How do we know if a feature belongs here?

- What's the broad purpose of the feature? If the answer includes data or database,
  then it probably belongs here.

The data model is a core part of Wasp. Why is this separate from Essentials?

- Some parts of the data model are "extras." Projects will use some of them, but
  it varies from project to project.

### Advanced Features

Answers: What else does Wasp have to offer?

How do we know if a feature belongs here vs somewhere else?

- It's not a core feature.
- It's one conceptual "unit." As in, the feature doesn't have several complex
  subcomponents (like how auth has several providers requiring long explanation
  of how to use each one).

### Authentication

Answers: I know auth exists in Wasp, now tell me all the options!

--> Why is this not an Advanced Feature? Auth providers have large differences
in use and how they are configured. Auth UI is also a large topic, as well
as best practices for using auth.

### Project Setup

Answers: How do I add/configure X in my project?

--> Why are these not Advanced Features? Each are pretty boring and small, and
not really an interesting feature of Wasp, just something that can be done.
And they all relate to configuring the project/what can exist within the
project.

## Examples

### Code blocks

We've created a couple of custom Docusaurus plugins to make authoring code blocks easier and more consistent.
These plugins are explained below:

#### `auto-js`: write a TypeScript example and auto-generate the JavaScript version

For examples that need to have both a JavaScript and TypeScript version, you can write only the TypeScript version and add an `auto-js` meta attribute to the code block, like so:

````mdx
```ts title="src/apis.ts" auto-js
export const validatePassword = (password: string) => password.length > 8;
```
````

And it will automatically generate a JS version by stripping the types from the TypeScript code, and add a selector to switch between them:

````mdx
<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/apis.js"
export const validatePassword = (password) => password.length > 8;
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/apis.ts"
export const validatePassword = (password: string) => password.length > 8;
```

</TabItem>
</Tabs>
````

This Docusaurus plugin is implemented in [./src/remark/auto-js-code.ts](./src/remark/auto-js-code.ts).

##### Caveats

`auto-js` specifically is backed by [ts-blank-space](https://github.com/bloomberg/ts-blank-space), which will _only_ remove the type annotations and not process anything else. Thus, some edge-cases can arise, so we recommend to run `npm run start` and check the output JS in the browser to see if everything looks good.

Known caveats are:

- `auto-js` will run `prettier` on your code due to how the TS->JS transfomration works. You can copy the generated code back to the file to be consistent between how it's written and how it will be displayed.
- `import`s that only import types will not be removed from the generated JS unless you use the `type` specifier in the import statement.
- A `// highlight-next-line` comment before a TS-only line will remove the line but not the comment, which will hang around and highlight the wrong line. Use `// highlight-start` and `// highlight-end` instead.
- The plugin will not replace file names inside the code block (e.g. imports or clarification comments). This is only really a problem in the tutorial pages. There is no solution for this at the moment.

If any of these caveats get in the way of correctly expressing the code you need, you can always write the JS version manually, as explained in the next section. The `auto-js` plugin is just a convenience to avoid writing the same code twice.

#### Manually creating a language switcher

You can create a language switcher manually as described in [Docusaurus docs](https://docusaurus.io/docs/markdown-features/code-blocks#multi-language-support-code-blocks).

#### `with-hole`: omit part of the code

If you need to omit some part of the code in a code example, you can use the `with-hole` meta attribute which will add an ellipsis wherever you write the identifier `$HOLE$` in the code block, so you can keep it syntactically valid. You can combine it with the `auto-js` tag.

For example, the following input:

````mdx
```ts title="src/apis.ts" auto-js with-hole
export const validatePassword = (password: string) =>
  password.length > 8 && $HOLE$;
```
````

Will be transformed to:

````mdx
<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/apis.js"
export const validatePassword = (password) => password.length > 8 && /* ... */;
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/apis.ts"
export const validatePassword = (password: string) => password.length > 8 && /* ... */;
```

</TabItem>
</Tabs>
````

This Docusaurus plugin is implemented in [./src/remark/code-with-hole.ts](./src/remark/code-with-hole.ts).
