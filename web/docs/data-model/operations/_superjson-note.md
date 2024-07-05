import { ShowForTs } from '@site/src/components/TsJsHelpers';

:::info Payload constraints 
Wasp uses [superjson](https://github.com/blitz-js/superjson) under the hood.
This means you're not limited to only sending and receiving JSON payloads.

You can send and receive any superjson-compatible payload (like Dates, Sets, Lists, circular references, etc.) and let Wasp handle the (de)serialization.

<ShowForTs>

As long as you're annotating your Operations with the correct automatically generated types, TypeScript ensures your payloads are valid (i.e., Wasp knows how to serialize and deserialize them).
</ShowForTs>

:::
