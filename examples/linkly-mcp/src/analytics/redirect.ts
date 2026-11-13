// Public GET /r/:slug: records a click and redirects. Deliberately not a
// service call with a Principal: the visitor is anonymous.
import type { Request, Response } from "express";
import { prisma } from "wasp/server";
import type { Redirect } from "wasp/server/api";
import { recordClick, resolveRedirect } from "./clicks";

export const redirect: Redirect = async (req: Request, res: Response) => {
  const target = await resolveRedirect(prisma, String(req.params.slug ?? ""));
  if (!target) {
    res.status(404).type("html").send(NOT_FOUND_HTML);
    return;
  }
  // Don't make the visitor wait for analytics.
  void recordClick(prisma, target.linkId, {
    referer: req.get("referer"),
    userAgent: req.get("user-agent"),
  }).catch((e) => console.error("Failed to record click", e));
  res.redirect(302, target.destinationUrl);
};

const NOT_FOUND_HTML = `<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Link not found · Linkly</title>
<style>body{font-family:system-ui,sans-serif;display:grid;place-items:center;min-height:100vh;margin:0;color:#111}
main{text-align:center;padding:2rem}h1{font-size:1.5rem;margin:0 0 .5rem}p{color:#555}</style></head>
<body><main><h1>This short link doesn't exist</h1><p>It may have been deleted or archived.</p></main></body></html>`;
