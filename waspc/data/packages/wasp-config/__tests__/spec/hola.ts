import { app, page, route } from "../../src/spec/publicApi/index.js";

export default app({
  name: "hola",
  wasp: { version: "0.23.0" },
  title: "My Hola App",
  auth: {
    userEntity: "User",
    methods: {
      email: {
        fromField: { email: "my@app.com" },
        emailVerification: { clientRoute: "/sdafouijnsdu" }, // <- this fails
        passwordReset: { clientRoute: "/hola" }, // <- we get autocompletion
      },
    },
    onAuthFailedRedirectTo: "/hola",
  },
  parts: [
    route("hola", "/hola", page({ import: "foo", from: "@src/bar" })),
    route("adios", "/adios", page({ import: "baz", from: "@src/qux" })),
  ],
});
