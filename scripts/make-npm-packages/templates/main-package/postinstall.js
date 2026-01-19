sendAnalytics().catch(() => {});

async function sendAnalytics() {
  // This is a public API key for PostHog analytics - it's safe to include in plain text
  const POSTHOG_API_KEY = "CdDd2A0jKTI2vFAsrI9JWm3MqpOcgHz1bMyogAcwsE4";
  const POSTHOG_URL = "https://app.posthog.com/capture";
  const TIMEOUT_MS = 500;

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
      context:
        (process.env.WASP_TELEMETRY_CONTEXT ?? "") + (isCI() ? " CI" : ""),
    },
  };

  await fetch(POSTHOG_URL, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(payload),
    signal: AbortSignal.timeout(TIMEOUT_MS),
  });
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
    case "win32":
      return "windows";
    default:
      return "Unknown";
  }
}

function isCI() {
  // List of common CI environment variables, keep in sync with the get-wasp-sh/installer.sh.
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

  return CI_ENV_VARS.some((envVar) => process.env[envVar]);
}
