#!/usr/bin/env npx tsx

/**
 * upload-youtube.ts — Upload videos to YouTube via the Data API v3
 *
 * Usage (CLI):
 *   npx tsx upload-youtube.ts --auth                              # One-time OAuth setup
 *   npx tsx upload-youtube.ts <video> --title "..." [options]     # Upload a video
 *
 * Usage (module):
 *   import { authorize, uploadVideo } from "./upload-youtube.js";
 *   await authorize();
 *   const result = await uploadVideo("video.mp4", { title: "My Video" });
 *
 * Environment:
 *   YOUTUBE_CLIENT_ID     — OAuth 2.0 client ID from Google Cloud Console
 *   YOUTUBE_CLIENT_SECRET — OAuth 2.0 client secret from Google Cloud Console
 */

import { exec } from "node:child_process";
import {
  chmodSync,
  createReadStream,
  existsSync,
  readFileSync,
  statSync,
  writeFileSync,
} from "node:fs";
import {
  createServer,
  type IncomingMessage,
  type ServerResponse,
} from "node:http";
import { homedir } from "node:os";
import { join, resolve } from "node:path";
import { URL, URLSearchParams, fileURLToPath } from "node:url";
import { parseArgs } from "node:util";

// ─── Configuration ──────────────────────────────────────────────────────────

const TOKEN_FILE = join(homedir(), ".youtube-upload-tokens.json");
const REDIRECT_PORT = 9004;
const REDIRECT_URI = `http://127.0.0.1:${REDIRECT_PORT}`;
const AUTH_URL = "https://accounts.google.com/o/oauth2/v2/auth";
const TOKEN_URL = "https://oauth2.googleapis.com/token";
const UPLOAD_URL = "https://www.googleapis.com/upload/youtube/v3/videos";
const CHANNELS_URL = "https://www.googleapis.com/youtube/v3/channels";
const SCOPES = [
  "https://www.googleapis.com/auth/youtube.upload",
  "https://www.googleapis.com/auth/youtube.readonly",
].join(" ");
/** YouTube category: "People & Blogs" */
const YOUTUBE_CATEGORY_PEOPLE_BLOGS = "22";
/** Owner read/write only — keeps OAuth tokens private */
const TOKEN_FILE_PERMISSIONS = 0o600;

// ─── Types ──────────────────────────────────────────────────────────────────

interface TokenResponse {
  access_token?: string;
  refresh_token?: string;
  error?: string;
  error_description?: string;
}

interface UploadOptions {
  title?: string;
  description?: string;
  privacy?: "private" | "unlisted" | "public";
  tags?: string[];
}

interface UploadResult {
  videoId: string;
  url: string;
  studioUrl: string;
}

// ─── Helpers ────────────────────────────────────────────────────────────────

function getClientCredentials(): { clientId: string; clientSecret: string } {
  const clientId = process.env.YOUTUBE_CLIENT_ID;
  const clientSecret = process.env.YOUTUBE_CLIENT_SECRET;
  if (!clientId || !clientSecret) {
    throw new Error(
      "YOUTUBE_CLIENT_ID and YOUTUBE_CLIENT_SECRET must be set in environment",
    );
  }
  return { clientId, clientSecret };
}

function openBrowser(url: string): void {
  const cmd =
    process.platform === "darwin"
      ? `open "${url}"`
      : process.platform === "win32"
        ? `start "${url}"`
        : `xdg-open "${url}"`;
  exec(cmd, (err) => {
    if (err) console.warn("Could not open browser automatically.");
  });
}

// ─── OAuth: capture authorization code via local HTTP server ────────────────

function captureAuthCode(): Promise<string> {
  return new Promise((resolve, reject) => {
    const server = createServer((req: IncomingMessage, res: ServerResponse) => {
      const url = new URL(req.url || "/", `http://127.0.0.1:${REDIRECT_PORT}`);
      const code = url.searchParams.get("code") || "";
      const error = url.searchParams.get("error") || "";

      res.writeHead(200, { "Content-Type": "text/html" });
      if (error) {
        res.end("<h1>Authorization failed</h1><p>You can close this tab.</p>");
      } else {
        res.end("<h1>Authorized!</h1><p>You can close this tab.</p>");
      }

      server.close();

      if (error) {
        reject(new Error(`OAuth error: ${error}`));
      } else if (!code) {
        reject(new Error("No authorization code received"));
      } else {
        resolve(code);
      }
    });

    server.listen(REDIRECT_PORT, "127.0.0.1", () => {
      // Server ready
    });

    server.on("error", reject);
  });
}

// ─── authorize() — interactive OAuth flow ───────────────────────────────────

export async function authorize(): Promise<void> {
  const { clientId, clientSecret } = getClientCredentials();

  const params = new URLSearchParams({
    client_id: clientId,
    redirect_uri: REDIRECT_URI,
    response_type: "code",
    scope: SCOPES,
    access_type: "offline",
    prompt: "consent",
  });

  const authUrl = `${AUTH_URL}?${params}`;

  console.log("Opening browser for Google authorization...\n");
  console.log("If the browser doesn't open, visit this URL:");
  console.log(authUrl + "\n");

  openBrowser(authUrl);

  console.log(`Waiting for authorization on port ${REDIRECT_PORT}...`);

  const code = await captureAuthCode();

  console.log("Exchanging code for tokens...");

  const resp = await fetch(TOKEN_URL, {
    method: "POST",
    headers: { "Content-Type": "application/x-www-form-urlencoded" },
    body: new URLSearchParams({
      code,
      client_id: clientId,
      client_secret: clientSecret,
      redirect_uri: REDIRECT_URI,
      grant_type: "authorization_code",
    }),
  });

  if (!resp.ok) {
    const body = await resp.text();
    throw new Error(`Token exchange failed (HTTP ${resp.status}): ${body}`);
  }

  const tokens: TokenResponse = await resp.json();

  if (!tokens.refresh_token) {
    throw new Error(`Token exchange failed: ${JSON.stringify(tokens)}`);
  }

  writeFileSync(TOKEN_FILE, JSON.stringify(tokens, null, 2));
  chmodSync(TOKEN_FILE, TOKEN_FILE_PERMISSIONS);

  console.log(`\nAuth complete! Tokens saved to ${TOKEN_FILE}`);

  // Verify which channel the tokens are associated with
  await verifyChannel();

  console.log("You can now upload videos.");
}

// ─── Refresh access token ───────────────────────────────────────────────────

async function getAccessToken(): Promise<string> {
  const { clientId, clientSecret } = getClientCredentials();

  if (!existsSync(TOKEN_FILE)) {
    throw new Error("Not authenticated. Run: npx tsx upload-youtube.ts --auth");
  }

  const stored: TokenResponse = JSON.parse(readFileSync(TOKEN_FILE, "utf-8"));
  if (!stored.refresh_token) {
    throw new Error(
      "Invalid token file. Run: npx tsx upload-youtube.ts --auth",
    );
  }

  const resp = await fetch(TOKEN_URL, {
    method: "POST",
    headers: { "Content-Type": "application/x-www-form-urlencoded" },
    body: new URLSearchParams({
      client_id: clientId,
      client_secret: clientSecret,
      refresh_token: stored.refresh_token,
      grant_type: "refresh_token",
    }),
  });

  if (!resp.ok) {
    const body = await resp.text();
    throw new Error(`Token refresh failed (HTTP ${resp.status}): ${body}`);
  }

  const tokens: TokenResponse = await resp.json();

  if (!tokens.access_token) {
    throw new Error(`Token refresh failed: ${JSON.stringify(tokens)}`);
  }

  return tokens.access_token;
}

// ─── verifyChannel() — confirm which YouTube channel the tokens belong to ───

export async function verifyChannel(
  existingToken?: string,
): Promise<{ id: string; title: string; handle: string }> {
  const token = existingToken ?? (await getAccessToken());

  const resp = await fetch(`${CHANNELS_URL}?part=snippet&mine=true`, {
    headers: { Authorization: `Bearer ${token}` },
  });

  if (!resp.ok) {
    const body = await resp.text();
    throw new Error(
      `Channels API request failed (HTTP ${resp.status}): ${body}`,
    );
  }

  const data = (await resp.json()) as Record<string, unknown>;
  const items = data.items as Array<Record<string, unknown>> | undefined;

  if (!items || items.length === 0) {
    throw new Error(
      "No YouTube channel found for this account. Did you select the right channel during --auth?",
    );
  }

  const channel = items[0];
  const snippet = channel.snippet as Record<string, unknown>;

  const info = {
    id: channel.id as string,
    title: snippet.title as string,
    handle: (snippet.customUrl as string) || "",
  };

  console.log(
    `Authenticated channel: ${info.title} (${info.handle || info.id})`,
  );
  return info;
}

// ─── uploadVideo() — resumable upload ───────────────────────────────────────

export async function uploadVideo(
  filePath: string,
  options: UploadOptions = {},
): Promise<UploadResult> {
  if (!existsSync(filePath)) {
    throw new Error(`File not found: ${filePath}`);
  }

  const title =
    options.title || filePath.replace(/^.*[\\/]/, "").replace(/\.[^.]*$/, "");
  const description = options.description || "";
  const privacy = options.privacy || "private";
  const tags = options.tags ?? [];

  console.log("Getting access token...");
  const token = await getAccessToken();

  await verifyChannel(token);

  const metadata = JSON.stringify({
    snippet: {
      title,
      description,
      tags,
      categoryId: YOUTUBE_CATEGORY_PEOPLE_BLOGS,
    },
    status: {
      privacyStatus: privacy,
    },
  });

  // Step 1: Initiate resumable upload
  console.log("Initiating upload...");
  const fileSize = statSync(filePath).size;

  const initResp = await fetch(
    `${UPLOAD_URL}?uploadType=resumable&part=snippet,status`,
    {
      method: "POST",
      headers: {
        Authorization: `Bearer ${token}`,
        "Content-Type": "application/json; charset=UTF-8",
        "X-Upload-Content-Type": "video/*",
        "X-Upload-Content-Length": String(fileSize),
      },
      body: metadata,
    },
  );

  const uploadUri = initResp.headers.get("location");
  if (!uploadUri) {
    const body = await initResp.text();
    throw new Error(`No upload URI returned. Response: ${body}`);
  }

  // Step 2: Upload the video file
  const sizeMb = Math.round(fileSize / 1048576);
  const fileName = filePath.replace(/^.*[\\/]/, "");
  console.log(`Uploading ${fileName} (~${sizeMb} MB)...`);

  const fileStream = createReadStream(filePath);
  const uploadResp = await fetch(uploadUri, {
    method: "PUT",
    headers: {
      "Content-Type": "video/*",
      "Content-Length": String(fileSize),
    },
    body: fileStream,
    duplex: "half",
  } as RequestInit);

  const result = (await uploadResp.json()) as Record<string, unknown>;
  const videoId = result.id as string | undefined;

  if (!videoId) {
    throw new Error(
      `Upload may have failed. Response: ${JSON.stringify(result)}`,
    );
  }

  const uploadResult: UploadResult = {
    videoId,
    url: `https://youtu.be/${videoId}`,
    studioUrl: `https://studio.youtube.com/video/${videoId}/edit`,
  };

  console.log(`\nUpload successful!`);
  console.log(`  ID:      ${uploadResult.videoId}`);
  console.log(`  URL:     ${uploadResult.url}`);
  console.log(`  Privacy: ${privacy}`);
  console.log(`  Studio:  ${uploadResult.studioUrl}`);

  return uploadResult;
}

// ─── CLI ────────────────────────────────────────────────────────────────────

function printHelp(): void {
  console.log(`upload-youtube.ts — Upload videos to YouTube via the Data API v3

Usage:
  npx tsx upload-youtube.ts --auth                              One-time OAuth setup
  npx tsx upload-youtube.ts --whoami                             Check authenticated channel
  npx tsx upload-youtube.ts <video> [options]                   Upload a video

Options:
  --title "..."         Video title (defaults to filename)
  --description "..."   Video description
  --privacy <level>     private | unlisted | public (default: private)
  --tags "t1,t2,t3"     Comma-separated tags

Environment variables:
  YOUTUBE_CLIENT_ID     OAuth client ID from Google Cloud Console
  YOUTUBE_CLIENT_SECRET OAuth client secret from Google Cloud Console

Setup:
  1. Create a Google Cloud project and enable YouTube Data API v3
  2. Create OAuth 2.0 credentials (Desktop app type)
  3. Add YOUTUBE_CLIENT_ID and YOUTUBE_CLIENT_SECRET to ~/.zshrc
  4. Run: npx tsx upload-youtube.ts --auth
  5. Authorize in the browser (one-time)`);
}

async function cli(): Promise<void> {
  const { values, positionals } = parseArgs({
    args: process.argv.slice(2),
    options: {
      help: { type: "boolean", short: "h", default: false },
      auth: { type: "boolean", default: false },
      whoami: { type: "boolean", default: false },
      title: { type: "string" },
      description: { type: "string" },
      privacy: { type: "string" },
      tags: { type: "string" },
    },
    allowPositionals: true,
    strict: true,
  });

  if (
    (positionals.length === 0 && !values.auth && !values.whoami) ||
    values.help
  ) {
    printHelp();
    return;
  }

  if (values.auth) {
    await authorize();
    return;
  }

  if (values.whoami) {
    await verifyChannel();
    return;
  }

  const file = positionals[0] ?? "";

  if (!file) {
    console.error("Error: No video file specified");
    printHelp();
    process.exit(1);
  }

  const opts: UploadOptions = {
    title: values.title,
    description: values.description,
    privacy: values.privacy as UploadOptions["privacy"],
    tags: values.tags
      ?.split(",")
      .map((t) => t.trim())
      .filter(Boolean),
  };

  await uploadVideo(file, opts);
}

// Run CLI if executed directly (not imported as a module)
const isDirectExecution =
  process.argv[1] &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url);

if (isDirectExecution) {
  cli().catch((err) => {
    console.error("Error:", err instanceof Error ? err.message : err);
    process.exit(1);
  });
}
