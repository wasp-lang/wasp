const POSTHOG_API_KEY = "CdDd2A0jKTI2vFAsrI9JWm3MqpOcgHz1bMyogAcwsE4";
const POSTHOG_URL = "https://app.posthog.com/capture";
const TIMEOUT_MS = 500;

const CI_ENV_VARS = [
  "BUILD_ID",
  "BUILD_NUMBER",
  "CI",
  "CI_APP_ID",
  "CI_BUILD_ID",
  "CI_BUILD_NUMBER",
  "CI_NAME",
  "CONTINUOUS_INTEGRATION",
  "GITHUB_ACTIONS",
  "RUN_ID",
  "TRAVIS",
];

sendAnalytics().catch(() => {});

async function sendAnalytics() {
  if (process.env.WASP_TELEMETRY_DISABLE) {
    return;
  }

  const payload = {
    api_key: POSTHOG_API_KEY,
    type: "capture",
    event: "npm-package:postinstall",
    distinct_id: generateDistinctId(),
    properties: {
      os: getOS(),
      context: isCI() ? "CI" : "",
    },
  };

  const controller = new AbortController();
  const timeoutId = setTimeout(() => controller.abort(), TIMEOUT_MS);

  try {
    await fetch(POSTHOG_URL, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload),
      signal: controller.signal,
    });
  } finally {
    clearTimeout(timeoutId);
  }
}

function generateDistinctId() {
  const random = Math.floor(Math.random() * 32768);
  const timestamp = Date.now();
  return `${random}${timestamp}`;
}

function getOS() {
  switch (process.platform) {
    case "linux":
      return "linux";
    case "darwin":
      return "osx";
    default:
      return "Unknown";
  }
}

function isCI() {
  return CI_ENV_VARS.some((envVar) => process.env[envVar]);
}
