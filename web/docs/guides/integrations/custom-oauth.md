---
comments: true
last_checked_with_versions:
  Wasp: "0.21"
---

# Custom OAuth Provider

This guide shows you how to implement a custom OAuth provider in your Wasp application. We'll use Spotify as an example, but the same approach works for any OAuth provider.

## Prerequisites

- A Wasp project with authentication set up
- An OAuth application registered with your provider (e.g., [Spotify Developer Dashboard](https://developer.spotify.com/dashboard))

## Setting up a Custom OAuth Provider

### 1. Configure main.wasp

Set up the auth configuration and API routes:

```wasp title="main.wasp"
app SpotifyOauth {
  wasp: {
    version: "^0.21.0"
  },
  title: "spotify-oauth",
  auth: {
    userEntity: User,
    onAuthFailedRedirectTo: "/",
    methods: {
      // highlight-start
      // Enable at least one OAuth provider so Wasp exposes OAuth helpers
      google: {}
      // highlight-end
    }
  },
}

route RootRoute { path: "/", to: MainPage }
page MainPage {
  component: import { MainPage } from "@src/MainPage",
}

// highlight-start
api authWithSpotify {
  httpRoute: (GET, "/auth/spotify"),
  fn: import { authWithSpotify } from "@src/auth",
  entities: []
}
// highlight-end

// highlight-start
api authWithSpotifyCallback {
  httpRoute: (GET, "/auth/spotify/callback"),
  fn: import { authWithSpotifyCallback } from "@src/auth",
  entities: []
}
// highlight-end
```

### 2. Configure environment variables

Create or update your `.env.server` file:

```bash title=".env.server"
SPOTIFY_CLIENT_ID=your_client_id
SPOTIFY_CLIENT_SECRET=your_client_secret

# You may need dummy values for built-in providers if you're using them
# just to get the arctic package installed
GOOGLE_CLIENT_ID=x
GOOGLE_CLIENT_SECRET=x
```

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

Create the authentication logic:

```ts title="src/auth.ts" auto-js
import * as arctic from "arctic";
import { config } from "wasp/server";
import {
  type AuthWithSpotify,
  type AuthWithSpotifyCallback,
} from "wasp/server/api";
import {
  createUser,
  findAuthIdentity,
  getRedirectUriForOneTimeCode,
  tokenStore,
  type ProviderName,
} from "wasp/server/auth";
import * as z from "zod";

if (!process.env.SPOTIFY_CLIENT_ID || !process.env.SPOTIFY_CLIENT_SECRET) {
  throw new Error(
    "Please provide SPOTIFY_CLIENT_ID and SPOTIFY_CLIENT_SECRET in .env.server file",
  );
}

const clientId = process.env.SPOTIFY_CLIENT_ID;
const clientSecret = process.env.SPOTIFY_CLIENT_SECRET;
const redirectURI = `${config.serverUrl}/auth/spotify/callback`;

const spotify = new arctic.Spotify(clientId, clientSecret, redirectURI);

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
  const accessToken = tokens.accessToken;
  const spotifyUser = await getSpotifyUser(accessToken);

  const providerId = {
    providerName: "spotify" as ProviderName,
    providerUserId: spotifyUser.id,
  };

  // Check if user already exists
  const existingUser = await findAuthIdentity(providerId);

  if (existingUser) {
    // Login existing user
    const authId = existingUser.authId;
    return redirectWithOneTimeToken(authId, res);
  } else {
    // Create new user
    const user = await createUser(providerId, JSON.stringify(spotifyUser), {
      name: spotifyUser.display_name,
      profilePicture:
        spotifyUser.images[1]?.url || spotifyUser.images[0]?.url || "",
    });
    const authId = user.auth!.id;
    return redirectWithOneTimeToken(authId, res);
  }
};

async function redirectWithOneTimeToken(
  authId: string,
  res: Parameters<AuthWithSpotifyCallback>[1],
) {
  const oneTimeCode = await tokenStore.createToken(authId);
  return res.redirect(getRedirectUriForOneTimeCode(oneTimeCode).toString());
}

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

type SpotifyUser = z.infer<typeof spotifyUserSchema>;

async function getSpotifyUser(accessToken: string): Promise<SpotifyUser> {
  const response = await fetch("https://api.spotify.com/v1/me", {
    headers: {
      Authorization: `Bearer ${accessToken}`,
    },
  });
  return spotifyUserSchema.parse(await response.json());
}
```

:::note
The `tokenStore` and `getRedirectUriForOneTimeCode` are internal Wasp APIs that may change in future versions. This guide relies on them because there is currently no public API for implementing fully custom OAuth flows.
:::

### 5. Create the login page

Add a login button that redirects to your OAuth endpoint:

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

## Configuring Your OAuth Provider

The Arctic library supports many providers. Check the [Arctic documentation](https://v1.arcticjs.dev/) for the full list and their specific setup requirements.

Each provider follows the same pattern - just replace the Spotify-specific code with the appropriate provider from Arctic.
