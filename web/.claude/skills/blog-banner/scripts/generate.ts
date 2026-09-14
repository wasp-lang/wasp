/**
 * Generate blog banner candidates with google/nano-banana-2 on Replicate.
 *
 * Usage:
 *   npx tsx .claude/skills/blog-banner/scripts/generate.ts \
 *     --prompt-file /tmp/prompt.txt \
 *     --out-dir /tmp/banners \
 *     [--count 2] [--resolution 1K] [--prefix candidate] \
 *     [--ref assets/da-boi.png] [--ref https://.../logo.png] \
 *     [--dry-run]
 *
 * Prints a JSON summary on stdout: { images: [...], cost: 0.134, ... }
 */

import { execFileSync } from "node:child_process";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { basename, extname, resolve } from "node:path";

const MODEL = "google/nano-banana-2";
const ASPECT_RATIO = "16:9"; // pinned: the model default is match_input_image
const OUTPUT_FORMAT = "png";

// Shared "Replicate API Token" item in the wasplang 1Password vault.
const OP_VAULT = "7caf4yfkhrcrno2ylcury2wezm";
const OP_ITEM = "iylwojjhgucrhe7ri7loszvhae";

const PRICE_PER_IMAGE: Record<string, number> = {
  "1K": 0.067,
  "2K": 0.101,
  "4K": 0.151,
};

const MIME_BY_EXT: Record<string, string> = {
  ".png": "image/png",
  ".jpg": "image/jpeg",
  ".jpeg": "image/jpeg",
  ".webp": "image/webp",
  ".gif": "image/gif",
};

type Args = {
  prompt: string;
  outDir: string;
  count: number;
  resolution: string;
  prefix: string;
  refs: string[];
  dryRun: boolean;
};

function parseArgs(argv: string[]): Args {
  const a: Args = {
    prompt: "",
    outDir: "",
    count: 2,
    resolution: "1K",
    prefix: "candidate",
    refs: [],
    dryRun: false,
  };
  for (let i = 0; i < argv.length; i++) {
    const v = () => {
      const next = argv[++i];
      if (next === undefined)
        throw new Error(`Missing value for ${argv[i - 1]}`);
      return next;
    };
    switch (argv[i]) {
      case "--prompt":
        a.prompt = v();
        break;
      case "--prompt-file":
        a.prompt = readFileSync(v(), "utf8").trim();
        break;
      case "--out-dir":
        a.outDir = v();
        break;
      case "--count":
        a.count = Number(v());
        break;
      case "--resolution":
        a.resolution = v();
        break;
      case "--prefix":
        a.prefix = v();
        break;
      case "--ref":
        a.refs.push(v());
        break;
      case "--dry-run":
        a.dryRun = true;
        break;
      default:
        throw new Error(`Unknown argument: ${argv[i]}`);
    }
  }
  if (!a.prompt) throw new Error("--prompt or --prompt-file is required");
  if (!a.outDir) throw new Error("--out-dir is required");
  if (!PRICE_PER_IMAGE[a.resolution]) {
    throw new Error(
      `--resolution must be one of ${Object.keys(PRICE_PER_IMAGE).join(", ")}`,
    );
  }
  if (!Number.isInteger(a.count) || a.count < 1)
    throw new Error("--count must be a positive integer");
  if (a.refs.length > 14)
    throw new Error("nano-banana-2 accepts at most 14 reference images");
  return a;
}

/** op first (per team decision), then the env var. */
function getToken(): string {
  for (const field of ["credential", "password"]) {
    try {
      const out = execFileSync(
        "op",
        ["read", `op://${OP_VAULT}/${OP_ITEM}/${field}`],
        {
          encoding: "utf8",
          stdio: ["ignore", "pipe", "ignore"],
        },
      ).trim();
      if (out) return out;
    } catch {
      // op missing, not signed in, or no such field - try the next option.
    }
  }
  const fromEnv = process.env.REPLICATE_API_TOKEN?.trim();
  if (fromEnv) return fromEnv;

  throw new Error(
    [
      "No Replicate API token.",
      "",
      "Preferred: 1Password CLI",
      "  brew install --cask 1password-cli",
      "  op signin   # wasplang account",
      `  op read "op://${OP_VAULT}/${OP_ITEM}/credential"   # should print the token`,
      "",
      "Fallback: export REPLICATE_API_TOKEN=<token>",
      '  Token lives in the shared 1Password vault as "Replicate API Token".',
    ].join("\n"),
  );
}

/**
 * Local files are uploaded to Replicate's Files API rather than inlined as data
 * URIs - Da Boi alone is ~700KB, and several refs would blow up the request body.
 * Returned URLs stay valid for about an hour, long enough to reuse across the pair.
 */
async function toRemoteUrl(token: string, ref: string): Promise<string> {
  if (/^https?:\/\//.test(ref)) return ref;

  const path = resolve(ref);
  const buf = readFileSync(path);
  const type =
    MIME_BY_EXT[extname(path).toLowerCase()] ?? "application/octet-stream";

  const form = new FormData();
  form.append("content", new Blob([buf], { type }), basename(path));

  const res = await fetch("https://api.replicate.com/v1/files", {
    method: "POST",
    headers: { Authorization: `Bearer ${token}` },
    body: form,
  });
  if (!res.ok)
    throw new Error(
      `File upload failed for ${ref}: ${res.status} ${await res.text()}`,
    );

  const json = (await res.json()) as { urls?: { get?: string } };
  const url = json.urls?.get;
  if (!url)
    throw new Error(
      `File upload for ${ref} returned no URL: ${JSON.stringify(json)}`,
    );
  return url;
}

const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));

async function predict(
  token: string,
  input: Record<string, unknown>,
): Promise<string> {
  const res = await fetch(
    `https://api.replicate.com/v1/models/${MODEL}/predictions`,
    {
      method: "POST",
      headers: {
        Authorization: `Bearer ${token}`,
        "Content-Type": "application/json",
        Prefer: "wait", // block until done (or ~60s) instead of polling from cold
      },
      body: JSON.stringify({ input }),
    },
  );
  if (!res.ok)
    throw new Error(`Prediction failed: ${res.status} ${await res.text()}`);

  let pred = (await res.json()) as {
    id: string;
    status: string;
    output?: string;
    error?: string;
    urls?: { get?: string };
  };

  // `Prefer: wait` covers the common case; keep polling if the model ran long.
  for (
    let i = 0;
    i < 90 && (pred.status === "starting" || pred.status === "processing");
    i++
  ) {
    await sleep(2000);
    const poll = await fetch(
      pred.urls?.get ?? `https://api.replicate.com/v1/predictions/${pred.id}`,
      {
        headers: { Authorization: `Bearer ${token}` },
      },
    );
    if (!poll.ok)
      throw new Error(`Polling failed: ${poll.status} ${await poll.text()}`);
    pred = (await poll.json()) as typeof pred;
  }

  if (pred.status !== "succeeded") {
    throw new Error(
      `Prediction ${pred.id} ${pred.status}: ${pred.error ?? "no error given"}`,
    );
  }
  if (typeof pred.output !== "string") {
    // The schema says a single URI; guard in case that ever changes.
    throw new Error(`Unexpected output shape: ${JSON.stringify(pred.output)}`);
  }
  return pred.output;
}

async function download(url: string, dest: string): Promise<number> {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`Download failed: ${res.status} ${url}`);
  const buf = Buffer.from(await res.arrayBuffer());
  writeFileSync(dest, buf);
  return buf.length;
}

async function main() {
  const args = parseArgs(process.argv.slice(2));
  const cost = +(PRICE_PER_IMAGE[args.resolution] * args.count).toFixed(3);

  if (args.dryRun) {
    console.log(
      JSON.stringify(
        {
          dryRun: true,
          model: MODEL,
          input: {
            prompt: args.prompt,
            aspect_ratio: ASPECT_RATIO,
            resolution: args.resolution,
            output_format: OUTPUT_FORMAT,
            image_input: args.refs,
          },
          count: args.count,
          estimatedCostUsd: cost,
        },
        null,
        2,
      ),
    );
    return;
  }

  const token = getToken();
  mkdirSync(args.outDir, { recursive: true });

  // Upload each ref once, then reuse the URLs across every prediction in the batch.
  const imageInput = await Promise.all(
    args.refs.map((r) => toRemoteUrl(token, r)),
  );

  const input = {
    prompt: args.prompt,
    aspect_ratio: ASPECT_RATIO,
    resolution: args.resolution,
    output_format: OUTPUT_FORMAT,
    ...(imageInput.length ? { image_input: imageInput } : {}),
  };

  const results = await Promise.allSettled(
    Array.from({ length: args.count }, async (_, i) => {
      const url = await predict(token, input);
      const path = resolve(args.outDir, `${args.prefix}-${i + 1}.png`);
      const bytes = await download(url, path);
      return { path, bytes, sourceUrl: url };
    }),
  );

  const images = results.flatMap((r) =>
    r.status === "fulfilled" ? [r.value] : [],
  );
  const errors = results.flatMap((r) =>
    r.status === "rejected" ? [String(r.reason?.message ?? r.reason)] : [],
  );

  console.log(
    JSON.stringify(
      {
        model: MODEL,
        resolution: args.resolution,
        aspectRatio: ASPECT_RATIO,
        referenceImages: args.refs,
        images,
        errors,
        // Failed predictions are not billed, so charge only for what came back.
        costUsd: +(PRICE_PER_IMAGE[args.resolution] * images.length).toFixed(3),
      },
      null,
      2,
    ),
  );

  if (!images.length) process.exit(1);
}

main().catch((err) => {
  console.error(err instanceof Error ? err.message : String(err));
  process.exit(1);
});
