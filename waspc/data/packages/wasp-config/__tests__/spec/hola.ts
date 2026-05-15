import { app, page, route } from "../../src/spec/publicApi/index.js";

const holaRoute = route(
  "hola",
  "/hola",
  page({ import: "foo", from: "@src/bar" }),
);

const adiosRoute = route(
  "adios",
  "/adios",
  page({ import: "baz", from: "@src/qux" }),
);

export default app({
  name: "hola",
  wasp: { version: "0.23.0" },
  title: "My Hola App",
  auth: {
    userEntity: "User",
    methods: {
      email: {
        fromField: { email: "my@app.com" },
        emailVerification: { clientRoute: "/sdafouijnsdu" },
        passwordReset: { clientRoute: adiosRoute },
      },
    },
    onAuthFailedRedirectTo: holaRoute,
  },
  parts: [holaRoute, adiosRoute],
});
