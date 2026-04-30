---
comments: true
last_checked_with_versions:
  Wasp: "0.21"
  swagger-jsdoc: "6"
  swagger-ui-express: "5"
---

# Swagger UI

This guide shows you how to add Swagger UI documentation to your Wasp APIs, making it easy to explore and test your endpoints.

## Setting up Swagger UI

### 1. Install dependencies

Install the required packages:

```bash
npm install swagger-jsdoc swagger-ui-express
npm install --save-dev @types/swagger-jsdoc @types/swagger-ui-express
```

### 2. Configure the API namespace in main.wasp

Add an API namespace for the Swagger UI endpoint:

```wasp title="main.wasp"
app MyApp {
  wasp: {
    version: "^0.21.0"
  },
  title: "my-app",
}

// highlight-start
apiNamespace swaggerUI {
  middlewareConfigFn: import { swaggerMiddleware } from "@src/swagger-ui",
  path: "/api-docs"
}
// highlight-end

api getStatus {
  fn: import { getStatus } from "@src/apis",
  auth: false,
  entities: [],
  httpRoute: (GET, "/status")
}
```

### 3. Create the spec generator script

Because `swagger-jsdoc` scans source files at runtime and `src/` is not available in the production Docker image, you need to pre-generate the spec as a TypeScript module that gets bundled with the server.

Create `scripts/generate-swagger.js`:

```js title="scripts/generate-swagger.js"
import swaggerJsdoc from "swagger-jsdoc";
import { writeFileSync } from "fs";

const spec = swaggerJsdoc({
  definition: {
    openapi: "3.0.0",
    info: {
      title: "My API",
      version: "1.0.0",
      description: "API documentation for my Wasp application",
      contact: {
        name: "API Support",
        url: "https://example.com",
        email: "support@example.com",
      },
    },
    components: {
      securitySchemes: {
        bearerAuth: {
          type: "http",
          scheme: "bearer",
          bearerFormat: "JWT",
          description: "Enter your JWT token in the format: Bearer {token}",
        },
      },
    },
    security: [{ bearerAuth: [] }],
  },
  apis: ["./src/**/*.ts", "!./src/swaggerSpec.ts"],
});

writeFileSync(
  "./src/swaggerSpec.ts",
  `const swaggerSpec = ${JSON.stringify(spec, null, 2)} as const;\nexport default swaggerSpec;\n`,
);
console.log("swaggerSpec.ts generated");
```

Run this script whenever you add or change JSDoc annotations:

```bash
node scripts/generate-swagger.js
```

This generates `src/swaggerSpec.ts`, which gets bundled into the server and works in both dev and production.

:::note
You may want to add `src/swaggerSpec.ts` to your `.gitignore` since it's a generated file.
:::

### 4. Create the Swagger middleware

Create the Swagger UI middleware that serves the pre-generated spec:

```ts title="src/swagger-ui.ts" auto-js
import * as express from "express";
import helmet from "helmet";
import swaggerUi from "swagger-ui-express";
import { env, MiddlewareConfigFn } from "wasp/server";
import baseSwaggerDoc from "./swaggerSpec";

const swaggerDoc = {
  ...baseSwaggerDoc,
  servers: [{ url: env.WASP_SERVER_URL, description: "API server" }],
};

export const swaggerMiddleware: MiddlewareConfigFn = (middlewareConfig) => {
  middlewareConfig.delete("helmet");
  middlewareConfig.set(
    "helmet",
    helmet({
      contentSecurityPolicy: false,
      hsts: false,
    }),
  );

  swaggerUi.serve.forEach((handler, i) => {
    middlewareConfig.set(`swaggerServe${i}`, handler);
  });

  middlewareConfig.set(
    "swaggerSetup",
    (
      req: express.Request,
      res: express.Response,
      next: express.NextFunction,
    ) => {
      return swaggerUi.setup(swaggerDoc, {
        explorer: true,
        customCss: ".swagger-ui .topbar { display: none }",
        swaggerOptions: {
          persistAuthorization: true,
          url: "/api-docs/swagger.json",
        },
      })(req, res, next);
    },
  );

  return middlewareConfig;
};
```

### 5. Document your APIs

Add JSDoc comments with Swagger annotations above your API definitions:

```ts title="src/apis.ts" auto-js
import { GetStatus } from "wasp/server/api";

/**
 * @swagger
 * /status:
 *   get:
 *     summary: Get API status
 *     description: Returns the current status of the API
 *     tags:
 *       - Status
 *     security:
 *       - bearerAuth: []
 *     responses:
 *       200:
 *         description: Successful response
 *         content:
 *           application/json:
 *             schema:
 *               type: object
 *               properties:
 *                 message:
 *                   type: string
 *       401:
 *         description: Unauthorized
 *       500:
 *         description: Server error
 */
export const getStatus: GetStatus = async (req, res) => {
  return res.json({ message: "OK" });
};
```

### 6. Access the documentation

Start your Wasp application and navigate to `http://localhost:3001/api-docs` to see your API documentation.

## Documenting Different Request Types

### POST Request with Body

```ts
/**
 * @swagger
 * /users:
 *   post:
 *     summary: Create a new user
 *     tags:
 *       - Users
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             type: object
 *             required:
 *               - email
 *               - password
 *             properties:
 *               email:
 *                 type: string
 *                 format: email
 *               password:
 *                 type: string
 *                 minLength: 8
 *     responses:
 *       201:
 *         description: User created successfully
 *       400:
 *         description: Invalid input
 */
```

### Path Parameters

```ts
/**
 * @swagger
 * /users/{id}:
 *   get:
 *     summary: Get user by ID
 *     tags:
 *       - Users
 *     parameters:
 *       - in: path
 *         name: id
 *         required: true
 *         schema:
 *           type: string
 *         description: User ID
 *     responses:
 *       200:
 *         description: User found
 *       404:
 *         description: User not found
 */
```

### Query Parameters

```ts
/**
 * @swagger
 * /users:
 *   get:
 *     summary: List users
 *     tags:
 *       - Users
 *     parameters:
 *       - in: query
 *         name: page
 *         schema:
 *           type: integer
 *           default: 1
 *         description: Page number
 *       - in: query
 *         name: limit
 *         schema:
 *           type: integer
 *           default: 10
 *         description: Items per page
 *     responses:
 *       200:
 *         description: List of users
 */
```

## Customization

### Custom Styling

You can customize the Swagger UI appearance:

```ts auto-js
swaggerUi.setup(swaggerDoc, {
  customCss: `
    .swagger-ui .topbar { display: none }
    .swagger-ui .info { margin: 20px 0 }
  `,
  customSiteTitle: "My API Documentation",
  customfavIcon: "/favicon.ico",
});
```

### Group APIs with Tags

Use tags to organize your endpoints. Add a `tags` array to the `definition` in `scripts/generate-swagger.js`:

```ts auto-js
const spec = swaggerJsdoc({
  definition: {
    // ... other config
    tags: [
      { name: "Users", description: "User management endpoints" },
      { name: "Posts", description: "Blog post endpoints" },
      { name: "Auth", description: "Authentication endpoints" },
    ],
  },
  apis: ["./src/**/*.ts", "!./src/swaggerSpec.ts"],
});
```

For more options, see the [swagger-jsdoc documentation](https://github.com/Surnet/swagger-jsdoc) and [swagger-ui-express documentation](https://github.com/scottie1984/swagger-ui-express).
