# Type Alias: EntityName

> **EntityName** = keyof `Entities` *extends* `never` ? `string` : `Extract`\<keyof `Entities`, `string`\>

One of the app's entity names, matching a model in `schema.prisma`.
