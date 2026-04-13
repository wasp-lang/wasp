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

  addRailwayBuildConfig(clientBuildArtefactsDir);

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

function addRailwayBuildConfig(buildDir: string): void {
  /*
    Our build configuration assumes we're using the Railpack builder. It is the
    default builder, but we'll add a minimal `railway.json` with the builder
    explicitly set to avoid any issues if Railway changes their defaults in the
    future.
    https://docs.railway.com/config-as-code/reference#specify-the-builder
  */
  fs.writeFileSync(path.join(buildDir, "railway.json"), railwayJsonContents);

  /*
    Railpack uses MPA routing by default, unless configured otherwise. We'll
    provide our own Caddyfile to override this behavior and enable SPA routing,
    which is required for Wasp apps to work correctly.
    https://railpack.com/languages/staticfile#custom-caddyfile

    (We could do this through a simpler `Staticfile` configuration, but this is
    a backport of a new feature that does need the `Caddyfile`, so I'll use it
    here too to reduce the divergence.)
  */
  fs.writeFileSync(path.join(buildDir, "Caddyfile"), caddyfileContents);
}

// NOTE: When updating this railway.json, make sure to also update it in the
// railway deployment docs at:
// web/docs/deployment/deployment-methods/paas.md
const railwayJsonContents = `{
  "$schema": "https://railway.com/railway.schema.json",
  "build": {
    "builder": "RAILPACK"
  }
}`;

// This Caddyfile is based on the default configuration provided by Railpack for
// static sites at:
// https://github.com/railwayapp/railpack/blob/main/core/providers/staticfile/Caddyfile.template
//
// NOTE: When updating this caddyfile, make sure to also update it in the
// railway deployment docs at:
// web/docs/deployment/deployment-methods/paas.md
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
	try_files {path} {path}.html {path}/index.html /index.html

	handle_errors {
		rewrite * /{err.status_code}.html
		file_server
	}
}
`;
