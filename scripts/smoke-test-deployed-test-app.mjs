// @ts-check

// This script performs smoke tests on deployed Wasp applications to verify
// that both the server and client app are running and accessible.
// Called from `.github/workflows/ci-deploy-test.yaml`.

const { serverUrl, clientUrl } = parseArgs();

runAppSmokeTest({
  serverUrl,
  clientUrl,
});

/**
 * Runs smoke tests against the deployed server and client applications.
 * Exits with code 1 if any test fails.
 * @param {{serverUrl: string, clientUrl: string}} urls - The URLs of the deployed server and client
 * @returns {Promise<void>}
 */
async function runAppSmokeTest({ serverUrl, clientUrl }) {
  await retryWithBackoff("Server", () => smokeTestServer(serverUrl));
  await retryWithBackoff("Client", () => smokeTestClient(clientUrl));

  log("All smoke tests passed");
}

/**
 * Tests the deployed server by hitting the /operations/get-date endpoint
 * and validating the JSON response structure.
 * @param {string} serverUrl - The base URL of the deployed server
 * @returns {Promise<void>}
 * @throws {Error} If the server doesn't respond correctly or returns unexpected data
 */
async function smokeTestServer(serverUrl) {
  const url = `${serverUrl}/operations/get-date`;
  log(`[Server] Hitting ${url}`);

  const response = await fetchWithTimeout(url, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
  });

  if (!response.ok) {
    throw new Error(`Server responded with status ${response.status}`);
  }

  const data = await response.json();

  if (!isValidGetDateResponse(data)) {
    throw new Error('Server response missing expected "json" field');
  }
}

/**
 * Validates the structure of the /operations/get-date response data.
 * @param {any} data - The response data to validate
 * @returns {boolean} True if valid, false otherwise
 */
function isValidGetDateResponse(data) {
  return data && typeof data === "object" && "json" in data;
}

/**
 * Tests the deployed client by fetching the root URL and checking
 * that the HTML contains the expected app title.
 * @param {string} clientUrl - The base URL of the deployed client
 * @returns {Promise<void>}
 * @throws {Error} If the client doesn't respond correctly or HTML doesn't contain expected text
 */
async function smokeTestClient(clientUrl) {
  log(`[Client] Hitting ${clientUrl}`);

  const response = await fetchWithTimeout(clientUrl, {
    method: "GET",
  });

  if (!response.ok) {
    throw new Error(`Client responded with status ${response.status}`);
  }

  const html = await response.text();

  const EXPECTED_CLIENT_TEXT = "Wasp Kitchen Sink";
  if (!html.includes(EXPECTED_CLIENT_TEXT)) {
    throw new Error(
      `Client HTML does not contain expected text '${EXPECTED_CLIENT_TEXT}'`,
    );
  }
}

/**
 * Retries a check function with exponential backoff.
 * @param {string} name - The name of the check for logging purposes
 * @param {() => Promise<void>} checkFn - The async function to retry
 * @returns {Promise<void>}
 * @throws {Error} If all retry attempts fail
 */
async function retryWithBackoff(name, checkFn) {
  const MAX_RETRIES = 5;
  const INITIAL_WAIT_SECONDS = 10;

  for (let attempt = 0; attempt < MAX_RETRIES; attempt++) {
    log(`[${name}] Attempt ${attempt + 1}/${MAX_RETRIES}`);

    try {
      await checkFn();
      log(`[${name}] Success`);
      return;
    } catch (error) {
      if (attempt < MAX_RETRIES - 1) {
        const waitTime = INITIAL_WAIT_SECONDS * Math.pow(2, attempt);
        log(`[${name}] Waiting ${waitTime}s before retry...`);
        await sleep(waitTime * 1000);
      } else {
        log(`[${name}] Failed after ${MAX_RETRIES} attempts`);
        throw error;
      }
    }
  }
}

/**
 * Performs a fetch request with a timeout.
 * @param {string} url - The URL to fetch
 * @param {RequestInit} options - Fetch options
 * @param {number} timeoutSeconds - Timeout in seconds
 * @returns {Promise<Response>} The fetch response
 * @throws {Error} If the request times out or fails
 */
async function fetchWithTimeout(url, options, timeoutSeconds = 30) {
  const controller = new AbortController();
  const timeoutId = setTimeout(() => controller.abort(), timeoutSeconds * 1000);

  try {
    const response = await fetch(url, {
      ...options,
      signal: controller.signal,
    });
    clearTimeout(timeoutId);
    return response;
  } catch (error) {
    clearTimeout(timeoutId);
    throw error;
  }
}

/**
 * Parses command line arguments to extract server and client hostnames.
 * @returns {{serverUrl: string, clientUrl: string}} The constructed HTTPS URLs for server and client
 * @throws {Error} If the required arguments are not provided
 */
function parseArgs() {
  const args = process.argv.slice(2);

  if (args.length !== 2) {
    console.error(
      "Usage: node smoke-test-deployed-test-app.mjs <server-hostname> <client-hostname>",
    );
    throw new Error("Invalid number of arguments");
  }

  const [serverHostname, clientHostname] = args;

  return {
    serverUrl: `https://${serverHostname}`,
    clientUrl: `https://${clientHostname}`,
  };
}

/**
 * Logs a message with a UTC timestamp in HH:MM:SS format.
 * @param {string} message - The message to log
 * @returns {void}
 */
function log(message) {
  const now = new Date();
  const timeHHMMSS = now.toISOString().substring(11, 19);
  console.log(`[${timeHHMMSS}] ${message}`);
}

/**
 * Sleeps for the specified duration.
 * @param {number} durationMs - Milliseconds to sleep
 * @returns {Promise<void>}
 */
function sleep(durationMs) {
  return new Promise((resolve) => setTimeout(resolve, durationMs));
}
