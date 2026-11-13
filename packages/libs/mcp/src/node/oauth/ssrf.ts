// Fetching a client's metadata document means making an HTTP request to a URL
// chosen by whoever starts an OAuth flow. These guards stop that request from
// reaching internal services (SSRF): the address is checked after DNS
// resolution, at connect time, so DNS rebinding can't slip past.
import dns from "node:dns";
import http from "node:http";
import https from "node:https";
import net, { BlockList } from "node:net";

export class SsrfError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "SsrfError";
  }
}

const LOOPBACK = new BlockList();
LOOPBACK.addSubnet("127.0.0.0", 8, "ipv4");
LOOPBACK.addAddress("::1", "ipv6");

const BLOCKED = new BlockList();
for (const [net4, prefix] of [
  ["0.0.0.0", 8], // "this" network
  ["10.0.0.0", 8], // private
  ["100.64.0.0", 10], // carrier-grade NAT
  ["127.0.0.0", 8], // loopback
  ["169.254.0.0", 16], // link-local (includes cloud metadata 169.254.169.254)
  ["172.16.0.0", 12], // private
  ["192.0.0.0", 24], // IETF protocol assignments
  ["192.168.0.0", 16], // private
  ["198.18.0.0", 15], // benchmarking
  ["224.0.0.0", 4], // multicast
  ["240.0.0.0", 4], // reserved + broadcast
] as const) {
  BLOCKED.addSubnet(net4, prefix, "ipv4");
}
for (const [net6, prefix] of [
  ["::", 128], // unspecified
  ["::1", 128], // loopback
  ["fc00::", 7], // unique local
  ["fe80::", 10], // link-local
  ["ff00::", 8], // multicast
  ["64:ff9b::", 96], // NAT64 (can wrap private IPv4)
] as const) {
  BLOCKED.addSubnet(net6, prefix, "ipv6");
}

export const LOOPBACK_HOSTNAMES = new Set([
  "localhost",
  "127.0.0.1",
  "[::1]",
  "::1",
]);

/** Is it OK to connect to this IP? Loopback only when the local-dev exception applies. */
export function isAddressAllowed(
  address: string,
  allowLoopback: boolean,
): boolean {
  let ip = address;
  let family = net.isIP(ip);
  if (family === 0) return false;
  // IPv4-mapped IPv6 (::ffff:10.0.0.1) is checked as IPv4.
  const mapped = /^::ffff:(\d+\.\d+\.\d+\.\d+)$/i.exec(ip);
  if (mapped) {
    ip = mapped[1]!;
    family = 4;
  }
  const type = family === 4 ? "ipv4" : "ipv6";
  if (allowLoopback && LOOPBACK.check(ip, type)) return true;
  return !BLOCKED.check(ip, type);
}

type LookupAll = (
  hostname: string,
) => Promise<{ address: string; family: number }[]>;
const systemLookupAll: LookupAll = (hostname) =>
  dns.promises.lookup(hostname, { all: true, verbatim: true });

/**
 * Resolves a hostname and rejects it if any address is blocked. Exported for
 * tests (with a fake resolver); fetchGuarded uses it at connect time.
 */
export async function resolveAllowed(
  hostname: string,
  allowLoopback: boolean,
  lookupAll: LookupAll = systemLookupAll,
): Promise<{ address: string; family: number }[]> {
  const bare = hostname.replace(/^\[|\]$/g, "");
  const addresses = net.isIP(bare)
    ? [{ address: bare, family: net.isIP(bare) }]
    : await lookupAll(bare);
  if (addresses.length === 0)
    throw new SsrfError(`Could not resolve ${hostname}.`);
  const blocked = addresses.find(
    (a) => !isAddressAllowed(a.address, allowLoopback),
  );
  if (blocked)
    throw new SsrfError(
      `Refusing to fetch ${hostname}: it resolves to a private or reserved address.`,
    );
  return addresses;
}

export type GuardedFetchOptions = {
  timeoutMs?: number;
  maxBytes?: number;
  allowLoopback?: boolean;
};

/**
 * GET a JSON document with SSRF guards: private/loopback/link-local addresses
 * rejected at connect time, 5s timeout, 64 KB body limit, no redirects.
 */
export function fetchJsonGuarded(
  url: URL,
  opts: GuardedFetchOptions = {},
): Promise<unknown> {
  const {
    timeoutMs = 5000,
    maxBytes = 64 * 1024,
    allowLoopback = false,
  } = opts;
  if (url.protocol !== "https:" && url.protocol !== "http:")
    return Promise.reject(new SsrfError("Only http(s) URLs can be fetched."));
  const client = url.protocol === "https:" ? https : http;

  const lookup: net.LookupFunction = (hostname, options, callback) => {
    resolveAllowed(hostname, allowLoopback).then(
      (addresses) => {
        // Every address was checked, so Node may try each of them (happy eyeballs).
        if ((options as dns.LookupOptions).all)
          (callback as unknown as (e: null, a: dns.LookupAddress[]) => void)(
            null,
            addresses,
          );
        else callback(null, addresses[0]!.address, addresses[0]!.family);
      },
      (err: Error) => callback(err as NodeJS.ErrnoException, "", 0),
    );
  };

  return new Promise((resolve, reject) => {
    const bareHost = url.hostname.replace(/^\[|\]$/g, "");
    const run = async () => {
      // IP literals skip `lookup`, so check them up front.
      if (net.isIP(bareHost)) await resolveAllowed(bareHost, allowLoopback);
      const req = client.request(
        url,
        {
          method: "GET",
          headers: {
            Accept: "application/json",
            "User-Agent": "Wasp-MCP-CIMD/1.0",
          },
          lookup,
          timeout: timeoutMs,
        },
        (res) => {
          if (res.statusCode && res.statusCode >= 300 && res.statusCode < 400) {
            res.resume();
            return reject(
              new SsrfError(
                "The client metadata URL redirected; redirects aren't followed.",
              ),
            );
          }
          if (res.statusCode !== 200) {
            res.resume();
            return reject(
              new SsrfError(
                `The client metadata URL returned HTTP ${res.statusCode}.`,
              ),
            );
          }
          let size = 0;
          const chunks: Buffer[] = [];
          res.on("data", (chunk: Buffer) => {
            size += chunk.length;
            if (size > maxBytes) {
              req.destroy();
              reject(
                new SsrfError(
                  `The client metadata document is larger than ${maxBytes / 1024} KB.`,
                ),
              );
              return;
            }
            chunks.push(chunk);
          });
          res.on("end", () => {
            try {
              resolve(JSON.parse(Buffer.concat(chunks).toString("utf8")));
            } catch {
              reject(
                new SsrfError("The client metadata document isn't valid JSON."),
              );
            }
          });
          res.on("error", reject);
        },
      );
      const timer = setTimeout(
        () =>
          req.destroy(
            new SsrfError("Timed out fetching the client metadata document."),
          ),
        timeoutMs,
      );
      req.on("close", () => clearTimeout(timer));
      req.on("timeout", () =>
        req.destroy(
          new SsrfError("Timed out fetching the client metadata document."),
        ),
      );
      req.on("error", reject);
      req.end();
    };
    run().catch(reject);
  });
}
