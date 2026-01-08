---
last_update:
  date: 2025-01-03
title: Custom OAuth Provider
comments: true
---

# Custom OAuth Provider

This guide shows you how to implement a custom OAuth provider in your Wasp application. We'll use Spotify as an example, but the same approach works for any OAuth provider.

## Prerequisites

- A Wasp project with authentication set up
- An OAuth application registered with your provider (e.g., [Spotify Developer Dashboard](https://developer.spotify.com/dashboard))

## Setting up a Custom OAuth Provider

### 1. Install dependencies

Install the Arctic library for OAuth handling and Zod for validation:

```bash
npm install arctic zod
```

### 2. Configure environment variables

Create or update your `.env.server` file:

```bash title=".env.server"
# Spotify OAuth credentials
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

### 4. Configure main.wasp

Set up the auth configuration and API routes:

```wasp title="main.wasp"
app SpotifyOauth {
  wasp: {
    version: "^0.15.0"
  },
  title: "spotify-oauth",
  client: {
    rootComponent: import { App } from "@src/App",
  },
  auth: {
    userEntity: User,
    onAuthFailedRedirectTo: "/",
    methods: {
      // Enable at least one OAuth provider so Wasp installs the `arctic` package
      google: {}
    }
  },
}

route RootRoute { path: "/", to: MainPage }
page MainPage {
  component: import { MainPage } from "@src/MainPage",
}

api authWithSpotify {
  httpRoute: (GET, "/auth/spotify"),
  fn: import { authWithSpotify } from "@src/auth",
  entities: []
}

api authWithSpotifyCallback {
  httpRoute: (GET, "/auth/spotify/callback"),
  fn: import { authWithSpotifyCallback } from "@src/auth",
  entities: []
}
```

### 5. Implement the OAuth handlers

Create the authentication logic:

```ts title="src/auth.ts"
import { AuthWithSpotify, AuthWithSpotifyCallback } from "wasp/server/api";
import { generateState, Spotify, SpotifyTokens } from "arctic";
import { config } from "wasp/server";
import { createUser, findAuthIdentity, ProviderName } from "wasp/auth/utils";
import * as z from "zod";
import { getRedirectUriForOneTimeCode, tokenStore } from "wasp/server/auth";

if (!process.env.SPOTIFY_CLIENT_ID || !process.env.SPOTIFY_CLIENT_SECRET) {
  throw new Error(
    "Please provide SPOTIFY_CLIENT_ID and SPOTIFY_CLIENT_SECRET in .env.server file",
  );
}

const clientId = process.env.SPOTIFY_CLIENT_ID;
const clientSecret = process.env.SPOTIFY_CLIENT_SECRET;
const redirectURI = `${config.serverUrl}/auth/spotify/callback`;

const spotify = new Spotify(clientId, clientSecret, redirectURI);

// Handler for /auth/spotify - initiates OAuth flow
export const authWithSpotify: AuthWithSpotify = async (req, res) => {
  const state = generateState();
  const url: URL = await spotify.createAuthorizationURL(state, {
    scopes: [], // Add scopes as needed, e.g., ["user-read-email"]
  });
  res.redirect(url.toString());
};

// Handler for /auth/spotify/callback - processes OAuth callback
export const authWithSpotifyCallback: AuthWithSpotifyCallback = async (
  req,
  res,
) => {
  const code = req.query.code as string;
  const tokens: SpotifyTokens = await spotify.validateAuthorizationCode(code);
  const spotifyUser = await getSpotifyUser(tokens.accessToken);

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
    const authId = user.auth.id;
    return redirectWithOneTimeToken(authId, res);
  }
};

async function redirectWithOneTimeToken(
  authId: string,
  res: Parameters<AuthWithSpotifyCallback>[1],
) {
  const oneTimeCode = await tokenStore.createToken(authId);
  // Redirect to Wasp's OAuth callback on the client side
  return res.redirect(getRedirectUriForOneTimeCode(oneTimeCode));
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
  const spotifyUser = spotifyUserSchema.parse(await response.json());
  return spotifyUser;
}
```

### 6. Create the login page

Add a login button that redirects to your OAuth endpoint:

```tsx title="src/MainPage.tsx"
import { logout, useAuth } from "wasp/client/auth";

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
            href="http://localhost:3001/auth/spotify"
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

### Spotify Setup

1. Go to the [Spotify Developer Dashboard](https://developer.spotify.com/dashboard)
2. Create a new application
3. Add `http://localhost:3001/auth/spotify/callback` as a redirect URI
4. Copy your Client ID and Client Secret to `.env.server`

### Other Providers

The Arctic library supports many providers. Check the [Arctic documentation](https://arcticjs.dev/) for the full list and their specific setup requirements.

Common providers include:

- GitHub
- Discord
- Twitter
- Apple
- Microsoft

Each provider follows the same pattern - just replace the Spotify-specific code with the appropriate provider from Arctic.
