// The app's declarations from its Wasp config, as Wasp reads them. Only
// generated in development, since they describe the app's internals.
export const decls = [
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
];
