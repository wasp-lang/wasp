import fs from "node:fs";
import { ReviewSchema } from "./schema.mjs";

const { inputPath, reviewdogOutputPath, summaryOutputPath } = parseArguments();
const review = readReview(inputPath);

writeReviewdogOutput(reviewdogOutputPath, review);
writeSummary(summaryOutputPath, review.summary);

function parseArguments() {
  const [inputPath, reviewdogOutputPath, summaryOutputPath] =
    process.argv.slice(2);

  if (inputPath && reviewdogOutputPath && summaryOutputPath) {
    return { inputPath, reviewdogOutputPath, summaryOutputPath };
  }

  throw new Error(
    "Usage: node to-rdjson.mjs <review.json> <reviewdog.json> <summary.md>",
  );
}

function readReview(inputPath) {
  const reviewJson =
    inputPath === "-"
      ? fs.readFileSync(process.stdin.fd, "utf8")
      : fs.readFileSync(inputPath, "utf8");
  return ReviewSchema.parse(JSON.parse(reviewJson));
}

function writeReviewdogOutput(outputPath, review) {
  const diagnostics = review.findings.map((finding) => ({
    message: `**${finding.title}**\n\n${finding.body}`,
    location: {
      path: finding.path,
      range: {
        start: { line: finding.startLine },
        end: { line: finding.endLine },
      },
    },
    severity: finding.severity,
  }));

  const reviewdogOutput = {
    source: { name: "code-review" },
    diagnostics,
  };

  fs.writeFileSync(outputPath, `${JSON.stringify(reviewdogOutput, null, 2)}\n`);
}

function writeSummary(outputPath, summary) {
  fs.writeFileSync(outputPath, `## Code review\n\n${summary}\n`);
}
