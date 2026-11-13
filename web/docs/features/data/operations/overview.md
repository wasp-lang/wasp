---
title: "Operations overview: queries and actions in Wasp"
sidebar_label: Overview
description: Operations are how a Wasp app works with its data. Queries read data, and Actions change it by updating existing entries or creating new ones.
---

import { Required } from '@site/src/components/Tag';

# Operations overview

While Entities enable you to define your app's data model and relationships, Operations are all about working with this data.

There are two kinds of Operations: [Queries](./queries.md) and [Actions](./actions.md). As their names suggest,
Queries are meant for reading data, and Actions are meant for changing it (either by updating existing entries or creating new ones).

Keep reading to find out all there is to know about Operations in Wasp.

## Keeping an operation on the server

By default, Wasp gives every Query and Action an HTTP route and a client-side function in `wasp/client/operations`, so your web app can call it. To keep one on the server, for example a job's helper or something only other operations call, set `expose: { client: false }`:

```ts title="main.wasp.ts"
query(getUsageReport, {
  entities: ["Task"],
  // highlight-next-line
  expose: { client: false },
}),
```

Wasp then generates no HTTP route and no `wasp/client/operations` export for it. Server code can still call it through `wasp/server/operations`.

