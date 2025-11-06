---
title: 3. Migrate DB
---

import { TutorialAction } from './TutorialAction';

## Migrate DB

<TutorialAction id="add-model" action="APPLY_PATCH">
Add a model to `schema.prisma`:

```prisma
model Post {
  id        Int      @id @default(autoincrement())
  title     String
  content   String?
  createdAt DateTime @default(now())
}
```

</TutorialAction>

Now, migrate the database with `wasp db migrate-dev`.

<TutorialAction id="migrate-db" action="MIGRATE_DB" />
