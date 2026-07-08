---
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";

# Custom OAuth Provider

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.24" }} />

This guide shows you how to implement a custom OAuth provider in your Wasp application. We'll use Spotify as an example, but the same approach works for any OAuth provider.

## Prerequisites

- A Wasp project with authentication set up
- An OAuth application registered with your provider (e.g., [Spotify Developer Dashboard](https://developer.spotify.com/dashboard))

## Setting up a Custom OAuth Provider

### 1. Configure main.wasp.ts

Set up the auth configuration and API routes:

```ts title="main.wasp.ts"
import { api, app, page, route } from "@wasp.sh/spec"
import { authWithSpotify, authWithSpotifyCallback } from "./src/auth" with { type: "ref" }
import { MainPage } from "./src/MainPage" with { type: "ref" }

const rootRoute = route("RootRoute", "/", page(MainPage))

export default app({
  name: "SpotifyOauth",
  wasp: { version: "^0.24.0" },
  title: "spotify-oauth",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  auth: {
    userEntity: "User",
    onAuthFailedRedirectTo: rootRoute,
    methods: {
      // highlight-start
      // Enable at least one OAuth provider so Wasp exposes OAuth helpers
      google: {},
      // highlight-end
    },
  },
  spec: [
    // highlight-start
    api("GET", "/auth/spotify", authWithSpotify),
    api("GET", "/auth/spotify/callback", authWithSpotifyCallback),
    // highlight-end
  ],
})
```

Note that `rootRoute` doesn't have to be listed in the `spec` array: referencing it from `auth.onAuthFailedRedirectTo` registers it automatically, just like pages passed to `route()`.

:::note
The route names are arbitrary, but the path on `authWithSpotifyCallback` must match the redirect URI you register with your provider. Spotify rejects `localhost` over HTTP, so register `http://127.0.0.1:3001/auth/spotify/callback` in the [Spotify Developer Dashboard](https://developer.spotify.com/dashboard) and set Wasp's URLs to match in step 2.
:::

### 2. Configure environment variables

Create or update your `.env.server` file:

```bash title=".env.server"
SPOTIFY_CLIENT_ID=your_client_id
SPOTIFY_CLIENT_SECRET=your_client_secret

# You may need dummy values for built-in providers if you're using them
# just to get the arctic package installed
GOOGLE_CLIENT_ID=x
GOOGLE_CLIENT_SECRET=x

# Spotify rejects `localhost`, so point Wasp at 127.0.0.1 instead
WASP_SERVER_URL=http://127.0.0.1:3001
WASP_WEB_CLIENT_URL=http://127.0.0.1:3000
```

```bash title=".env.client"
REACT_APP_API_URL=http://127.0.0.1:3001
```

:::note
Most OAuth providers accept `localhost` directly; the `127.0.0.1` setup above is specific to Spotify. Other providers (e.g., Slack) require a real hostname. See [Local Network Testing](../debugging/local-network-testing.md) for the `nip.io` workaround.
:::

### 3. Set up the database schema

Update your Prisma schema to store the user data you need:

```prisma title="schema.prisma"
model User {
  id             String @id @default(cuid())
  name           String
  profilePicture String
}
```

### 4. Implement the OAuth handlers

The implementation splits across two files: pure Spotify logic in `src/spotify.ts`, and Wasp auth logic in `src/auth.ts`.

`src/spotify.ts` holds the [Arctic](https://v1.arcticjs.dev/providers/spotify) client (the library Wasp uses for OAuth) and the [Spotify `/me`](https://developer.spotify.com/documentation/web-api/reference/get-current-users-profile) profile fetch:

```ts title="src/spotify.ts" auto-js
import * as arctic from "arctic";
import { config } from "wasp/server";
import * as z from "zod";

if (!process.env.SPOTIFY_CLIENT_ID || !process.env.SPOTIFY_CLIENT_SECRET) {
  throw new Error(
    "Please provide SPOTIFY_CLIENT_ID and SPOTIFY_CLIENT_SECRET in .env.server file",
  );
}

const clientId = process.env.SPOTIFY_CLIENT_ID;
const clientSecret = process.env.SPOTIFY_CLIENT_SECRET;
const redirectURI = `${config.serverUrl}/auth/spotify/callback`;

export const spotify = new arctic.Spotify(clientId, clientSecret, redirectURI);

// Spotify user schema for validation
const spotifyUserSchema = z.object({
  id: z.string(),
  display_name: z.string(),
  external_urls: z.object({
    spotify: z.string(),
  }),
  images: z.array(
    z.object({
      url: z.string(),
      height: z.number(),
      width: z.number(),
    }),
  ),
});

export type SpotifyUser = z.infer<typeof spotifyUserSchema>;

export async function getSpotifyUser(accessToken: string): Promise<SpotifyUser> {
  const response = await fetch("https://api.spotify.com/v1/me", {
    headers: {
      Authorization: `Bearer ${accessToken}`,
    },
  });
  return spotifyUserSchema.parse(await response.json());
}
```

`src/auth.ts` wires the route handlers and uses `wasp/server/auth` helpers (`findAuthIdentity`, `createUser`) to connect the OAuth identity to a Wasp session (see [Custom Auth Actions](../../auth/advanced/custom-auth-actions.md) for details). This is similar to what Wasp does internally for [Google, GitHub and other supported providers](../../auth/social-auth/overview.md):

```ts title="src/auth.ts" auto-js
import * as arctic from "arctic";
import type {
  AuthWithSpotify,
  AuthWithSpotifyCallback,
} from "wasp/server/api";
import {
  createUser,
  findAuthIdentity,
  getRedirectUriForOneTimeCode,
  tokenStore,
} from "wasp/server/auth";
import type { ProviderName } from "wasp/server/auth";
import { spotify, getSpotifyUser, type SpotifyUser } from "./spotify";

// Handler for /auth/spotify - initiates OAuth flow
export const authWithSpotify: AuthWithSpotify = async (req, res) => {
  const state = arctic.generateState();
  const url = await spotify.createAuthorizationURL(state, { scopes: ["user-read-email"] });
  res.redirect(url.toString());
};

// Handler for /auth/spotify/callback - processes OAuth callback
export const authWithSpotifyCallback: AuthWithSpotifyCallback = async (
  req,
  res,
) => {
  const code = req.query.code as string;
  const tokens = await spotify.validateAuthorizationCode(code);
  const spotifyUser = await getSpotifyUser(tokens.accessToken);

  const providerId = {
    providerName: "spotify" as ProviderName,
    providerUserId: spotifyUser.id,
  };

  const existingIdentity = await findAuthIdentity(providerId);
  const authId = existingIdentity
    ? existingIdentity.authId
    : await createUserFromSpotifyProfile(providerId, spotifyUser);

  const oneTimeCode = await tokenStore.createToken(authId);
  return res.redirect(getRedirectUriForOneTimeCode(oneTimeCode).toString());
};

async function createUserFromSpotifyProfile(
  providerId: { providerName: ProviderName; providerUserId: string },
  spotifyUser: SpotifyUser,
): Promise<string> {
  const userData = {
    name: spotifyUser.display_name,
    profilePicture:
      spotifyUser.images[1]?.url ?? spotifyUser.images[0]?.url ?? "",
  };
  const user = await createUser(providerId, JSON.stringify(spotifyUser), userData);
  return user.auth!.id;
}
```

:::note
The `tokenStore` and `getRedirectUriForOneTimeCode` are internal Wasp APIs that may change in future versions. This guide relies on them because there is currently no public API for implementing fully custom OAuth flows.
:::

### 5. Create the login page

Add a login button that redirects to your OAuth endpoint. This follows the same pattern as Wasp's [custom social auth UI](../../auth/social-auth/create-your-own-ui.md):

```tsx title="src/MainPage.tsx" auto-js
import { logout, useAuth } from "wasp/client/auth";
import { config } from "wasp/client";

export const MainPage = () => {
  const { data: user } = useAuth();

  return (
    <div className="container">
      <main>
        {user ? (
          <div>
            <img src={user.profilePicture} alt="profile" />
            <br />
            Logged in as {user.name}
            <br />
            <button onClick={logout}>Log out</button>
          </div>
        ) : (
          <p>Not logged in</p>
        )}
        <div className="buttons">
          <a
            className="button button-filled"
            href={`${config.apiUrl}/auth/spotify`}
          >
            Login with Spotify
          </a>
        </div>
      </main>
    </div>
  );
};
```

## Using a Different OAuth Provider

[Arctic](https://v1.arcticjs.dev/) (v1) supports many providers. Check its documentation for the full list and their specific setup requirements.

Each provider follows the same pattern. For example, to use Twitch instead of Spotify:

- Swap the Arctic provider: `new arctic.Twitch(clientId, clientSecret, redirectURI)`.
- Fetch the user from `https://api.twitch.tv/helix/users` with the appropriate scopes (e.g., `user:read:email`).
- Update the `User` schema and the fields passed to `createUser` to match the provider's response.
