import fs from "node:fs";
import path from "node:path";

import { buildClient } from "../../../../common/clientApp.js";
import {
  displayWaspRocketImage,
  waspSays,
} from "../../../../common/terminal.js";
import { DeploymentInstructions } from "../../DeploymentInstructions.js";
import { clientAppPort, serverAppPort } from "../../ports.js";
import { generateServiceUrl } from "../../railwayService/url.js";
import { DeployCmdOptions } from "./DeployCmdOptions.js";
import {
  deployServiceWithStreamingLogs,
  ServiceDeploymentStatus,
} from "./common.js";

export async function deployClient({
  cmdOptions: options,
  serverServiceName,
  clientServiceName,
}: DeploymentInstructions<DeployCmdOptions>): Promise<void> {
  waspSays("Deploying your client now...");

  const serverServiceUrl =
    options.customServerUrl ??
    (await generateServiceUrl(serverServiceName, serverAppPort, options));

  const clientBuildArtefactsDir = await buildClient(serverServiceUrl, options);

  overrideRailwayCaddyfile(clientBuildArtefactsDir);

  const deploymentStatus = await deployServiceWithStreamingLogs(
    {
      name: clientServiceName,
      dirToDeploy: clientBuildArtefactsDir,
    },
    options,
  );

  displayWaspRocketImage();

  const clientUrl = await generateServiceUrl(
    clientServiceName,
    clientAppPort,
    options,
  );
  const messages: Record<ServiceDeploymentStatus, string> = {
    [ServiceDeploymentStatus.SUCCESS]: `Client has been deployed! Your Wasp app is accessible at: ${clientUrl}`,
    [ServiceDeploymentStatus.FAILED_TO_STREAM_LOGS]: `Client deployment started, but failed to stream build logs. Your Wasp app should be accessible at: ${clientUrl}`,
  };
  waspSays(messages[deploymentStatus]);
}

/**
 * Railway serves static sites with Caddy that by default
 * falls back to `index.html` for unknown routes.
 *
 * Since Wasp uses `200.html` as the fallback route by default,
 * and reserves `index.html` for * the prerendered `/` route,
 * we generate a custom Caddyfile into the build directory
 * so Railway picks it up instead.
 *
 * @see https://railpack.com/languages/staticfile#custom-caddyfile
 */
function overrideRailwayCaddyfile(buildDir: string): void {
  fs.writeFileSync(path.join(buildDir, "Caddyfile"), caddyfileContents);
}

// NOTE: When updating this caddyfile, make sure to also update it in the railway deployment docs.
/**
 * Closely follows the Railway's original Caddyfile.
 * The only diff is in the `try_files` directive.
 *
 * @see https://github.com/railwayapp/railpack/blob/main/core/providers/staticfile/Caddyfile.template
 */
const caddyfileContents = `{
  admin off
  persist_config off
  auto_https off

  log {
    format json
  }

  servers {
    trusted_proxies static private_ranges
  }
}

:{$PORT:80} {
  log {
    format json
  }

  respond /health 200

	# Security headers
	header {
		# Enable cross-site filter (XSS) and tell browsers to block detected attacks
		X-XSS-Protection "1; mode=block"
		# Prevent some browsers from MIME-sniffing a response away from the declared Content-Type
		X-Content-Type-Options "nosniff"
		# Keep referrer data off of HTTP connections
		Referrer-Policy "strict-origin-when-cross-origin"
		# Enable strict Content Security Policy
		Content-Security-Policy "default-src 'self'; img-src 'self' data: https: *; style-src 'self' 'unsafe-inline' https: *; script-src 'self' 'unsafe-inline' https: *; font-src 'self' data: https: *; connect-src 'self' https: *; media-src 'self' https: *; object-src 'none'; frame-src 'self' https: *;"
		# Remove Server header
		-Server
	}

  root * .

	# Handle static files
  file_server {
    hide .git
    hide .env*
  }

	# Compression with more formats
  encode {
    gzip
    zstd
  }

	# Try files with HTML extension and handle SPA routing
  try_files {path} {path}/index.html /200.html

	# Handle 404 errors
  handle_errors {
    rewrite * /{err.status_code}.html
    file_server
  }
}
`;
