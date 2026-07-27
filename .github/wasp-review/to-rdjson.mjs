import fs from "node:fs";
import path from "node:path";

const schema = JSON.parse(
  fs.readFileSync(new URL("./output-schema.json", import.meta.url), "utf8"),
);
const findingsSchema = schema.properties.findings;
const findingProperties = findingsSchema.items.properties;
const limits = {
  summary: schema.properties.summary.maxLength,
  findings: findingsSchema.maxItems,
  title: findingProperties.title.maxLength,
  body: findingProperties.body.maxLength,
  rule: findingProperties.rule.maxLength,
  path: findingProperties.path.maxLength,
};
const severities = new Set(findingProperties.severity.enum);

const [inputPath, rdjsonPath, summaryPath] = process.argv.slice(2);

if (!inputPath || !rdjsonPath || !summaryPath) {
  throw new Error(
    "Usage: node to-rdjson.mjs <review.json> <reviewdog.json> <summary.md>",
  );
}

const reviewJson =
  inputPath === "-"
    ? fs.readFileSync(process.stdin.fd, "utf8")
    : fs.readFileSync(inputPath, "utf8");
const review = JSON.parse(reviewJson);

if (
  review === null ||
  typeof review !== "object" ||
  !Array.isArray(review.findings)
) {
  throw new Error("Review output does not match the expected schema.");
}

if (review.findings.length > limits.findings) {
  throw new Error(`Review contains more than ${limits.findings} findings.`);
}

function validateString(value, label, maxLength) {
  if (typeof value !== "string" || value.trim().length === 0) {
    throw new Error(`${label} must be a non-empty string.`);
  }

  if (value.length > maxLength) {
    throw new Error(`${label} exceeds ${maxLength} characters.`);
  }

  return value;
}

function validatePath(value, label) {
  const filePath = validateString(value, `${label} path`, limits.path);
  const normalizedPath = path.posix.normalize(filePath);
  const isUnsafe =
    normalizedPath !== filePath ||
    normalizedPath === "." ||
    normalizedPath.startsWith("../") ||
    path.posix.isAbsolute(normalizedPath) ||
    path.win32.isAbsolute(filePath) ||
    filePath.includes("\\");

  if (isUnsafe) {
    throw new Error(`${label} has an unsafe path.`);
  }

  return normalizedPath;
}

const summary = validateString(
  review.summary,
  "Summary",
  limits.summary,
).trim();

const diagnostics = review.findings.map((finding, index) => {
  const label = `Finding ${index + 1}`;

  if (finding === null || typeof finding !== "object") {
    throw new Error(`${label} does not match the expected schema.`);
  }

  const title = validateString(finding.title, `${label} title`, limits.title);
  const body = validateString(finding.body, `${label} body`, limits.body);
  const rule = validateString(finding.rule, `${label} rule`, limits.rule);
  const findingPath = validatePath(finding.path, label);

  if (!Number.isInteger(finding.startLine) || finding.startLine < 1) {
    throw new Error(`${label} has an invalid startLine.`);
  }

  if (
    !Number.isInteger(finding.endLine) ||
    finding.endLine < finding.startLine
  ) {
    throw new Error(`${label} has an invalid endLine.`);
  }

  if (!severities.has(finding.severity)) {
    throw new Error(`${label} has an invalid severity.`);
  }

  return {
    message: `**${title}**\n\n${body}`,
    location: {
      path: findingPath,
      range: {
        start: { line: finding.startLine },
        end: { line: finding.endLine },
      },
    },
    severity: finding.severity,
    code: { value: rule },
  };
});

const rdjson = {
  source: { name: "wasp-review" },
  diagnostics,
};

fs.writeFileSync(rdjsonPath, `${JSON.stringify(rdjson, null, 2)}\n`);
fs.writeFileSync(summaryPath, `${summary}\n`);
