// The data that Wasp Studio shows, from the app's Wasp config. Only generated
// in development, since it describes the app's internals.
export const studioData = {
  "apis": [],
  "app": {
    "auth": null,
    "db": {
      "system": "SQLite"
    },
    "name": "waspApp"
  },
  "entities": [
    {
      "name": "Task"
    }
  ],
  "jobs": [],
  "operations": [],
  "pages": [
    {
      "authRequired": null,
      "name": "MainPage"
    }
  ],
  "routes": [
    {
      "name": "RootRoute",
      "path": "/",
      "toPage": {
        "name": "MainPage"
      }
    }
  ]
};
