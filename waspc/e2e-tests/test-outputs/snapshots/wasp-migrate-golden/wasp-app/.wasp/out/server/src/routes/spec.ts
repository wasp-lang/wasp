// The app's spec, as `wasp show spec --json` prints it. Only generated in
// development, since it describes the app's internals. Typed as `unknown` so
// TypeScript doesn't infer a type from the whole literal.
export const spec: unknown = {
  "dbSystem": "SQLite",
  "decls": [
    {
      "declType": "Entity",
      "declValue": {
        "fields": [
          {
            "name": "id",
            "type": "Int"
          },
          {
            "name": "description",
            "type": "String"
          },
          {
            "name": "isDone",
            "type": "Boolean"
          }
        ],
        "name": "Task"
      }
    },
    {
      "declType": "App",
      "declValue": {
        "auth": null,
        "client": null,
        "db": null,
        "deployment": null,
        "emailSender": null,
        "head": [
          "<link rel='icon' href='/favicon.ico' />"
        ],
        "name": "waspApp",
        "server": null,
        "title": "wasp-app",
        "wasp": {
          "version": "^0.27.0"
        },
        "webSocket": null
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": null,
        "component": {
          "alias": "MainPage",
          "kind": "named",
          "name": "MainPage",
          "path": "./src/MainPage"
        },
        "name": "MainPage"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "RootRoute",
        "path": "/",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "MainPage"
        }
      }
    }
  ],
  "waspVersion": "0.27.0"
};
