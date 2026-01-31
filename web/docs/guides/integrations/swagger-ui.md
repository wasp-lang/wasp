---
title: Swagger UI
comments: true
last_checked_with_versions:
  Wasp: "0.16"
---

# Swagger UI

This guide shows you how to add Swagger UI documentation to your Wasp APIs, making it easy to explore and test your endpoints.

## Prerequisites

Make sure you have a Wasp project set up. If you haven't, follow the [Getting Started](../../introduction/quick-start.md) guide first.

## Setting up Swagger UI

### 1. Install dependencies

Install the required packages:

```bash
npm install swagger-jsdoc swagger-ui-express helmet
npm install --save-dev @types/swagger-jsdoc @types/swagger-ui-express
```

### 2. Configure the API namespace in main.wasp

Add an API namespace for the Swagger UI endpoint:

```wasp title="main.wasp"
app MyApp {
  wasp: {
    version: "^0.16.0"
  },
  title: "my-app",
}

apiNamespace swaggerUI {
  middlewareConfigFn: import { swaggerMiddleware } from "@src/swagger-ui",
  path: "/api-docs"
}

// Example API endpoint with Swagger documentation
api barBaz {
  fn: import { barBaz } from "@src/apis",
  auth: false,
  entities: [],
  httpRoute: (GET, "/bar/baz")
}
```

### 3. Create the Swagger middleware

Create the Swagger UI configuration and middleware:

```ts title="src/swagger-ui.ts"
import * as express from "express";
import helmet from "helmet";
import swaggerJsdoc from "swagger-jsdoc";
import swaggerUi from "swagger-ui-express";
import { MiddlewareConfigFn } from "wasp/server";

// Swagger configuration
const swaggerDoc = swaggerJsdoc({
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
    servers: [
      {
        url: process.env.WASP_SERVER_URL || "http://localhost:3001",
        description: "API server",
      },
    ],
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
    security: [
      {
        bearerAuth: [],
      },
    ],
  },
  // Parse JSDoc comments from these files
  apis: ["../../**/*.wasp*"],
});

export const swaggerMiddleware: MiddlewareConfigFn = (middlewareConfig) => {
  // Use a custom Helmet configuration with CSP disabled for Swagger UI
  middlewareConfig.delete("helmet");
  middlewareConfig.set(
    "helmet",
    helmet({
      contentSecurityPolicy: false,
      hsts: false,
    }),
  );

  // Register Swagger middleware
  middlewareConfig.set(
    "swaggerServe",
    (
      req: express.Request,
      res: express.Response,
      next: express.NextFunction,
    ) => {
      const applyMiddleware = (index: number) => {
        if (index >= swaggerUi.serve.length) {
          return next();
        }
        swaggerUi.serve[index](req, res, () => applyMiddleware(index + 1));
      };
      applyMiddleware(0);
    },
  );

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

  console.log(
    `Swagger UI available at ${process.env.WASP_SERVER_URL || "http://localhost:3001"}/api-docs`,
  );

  return middlewareConfig;
};
```

### 4. Document your APIs

Add JSDoc comments with Swagger annotations above your API definitions:

```ts title="src/apis.ts"
import { BarBaz } from "wasp/server/api";

/**
 * @swagger
 * /bar/baz:
 *   get:
 *     summary: Get bar baz data
 *     description: Returns some example data
 *     tags:
 *       - Bar
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
export const barBaz: BarBaz = async (req, res) => {
  return res.json({ message: "Hello from bar baz!" });
};
```

### 5. Access the documentation

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

```ts
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

Use tags to organize your endpoints:

```ts
const swaggerDoc = swaggerJsdoc({
  definition: {
    // ... other config
    tags: [
      { name: "Users", description: "User management endpoints" },
      { name: "Posts", description: "Blog post endpoints" },
      { name: "Auth", description: "Authentication endpoints" },
    ],
  },
  apis: ["../../**/*.wasp*"],
});
```

For more options, see the [swagger-jsdoc documentation](https://github.com/Surnet/swagger-jsdoc) and [swagger-ui-express documentation](https://github.com/scottie1984/swagger-ui-express).
