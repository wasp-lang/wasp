// @ts-check

// Cleans up a Railway project by name.
// Called from: .github/workflows/ci-deploy-test.yaml

const { projectName } = parseArgs();
await cleanupRailwayProject(projectName);

/**
 * Cleans up a Railway project by finding and deleting it.
 * @param {string} projectName - Name of the project to delete
 * @returns {Promise<void>}
 */
async function cleanupRailwayProject(projectName) {
  log(`Looking for project: ${projectName}`);

  const projectId = await getRailwayProjectId(projectName);

  if (!projectId) {
    log(`Project ${projectName} not found, skipping cleanup`);
    return;
  }

  log(`Found project ${projectName} with ID: ${projectId}`);
  await deleteRailwayProject(projectId);
  log(`Successfully deleted project ${projectName}`);
}

/**
 * Gets a Railway project ID by name.
 * @param {string} projectName - Name of the project to find
 * @returns {Promise<string | null>} Project ID or null if not found
 */
async function getRailwayProjectId(projectName) {
  const workspaceId = getRequiredEnvVar("RAILWAY_WASP_WORKSPACE_ID");

  const query = `
    query($workspaceId: String!) {
      workspace(workspaceId: $workspaceId) {
        projects {
          edges {
            node {
              id
              name
            }
          }
        }
      }
    }
  `;

  const data = await railwayGraphQL(query, { workspaceId });

  const project = data.workspace.projects.edges.find(
    /**
     * @param {any} edge
     */
    (edge) => edge.node.name === projectName,
  );

  return project?.node?.id ?? null;
}

/**
 * Deletes a Railway project by ID.
 * @param {string} projectId - ID of the project to delete
 * @returns {Promise<void>}
 */
async function deleteRailwayProject(projectId) {
  const mutation = `
    mutation($projectId: String!) {
      projectDelete(id: $projectId)
    }
  `;

  await railwayGraphQL(mutation, { projectId });
}

/**
 * Makes a GraphQL request to Railway API.
 * @param {string} query - GraphQL query or mutation
 * @param {Record<string, any>} [variables] - GraphQL variables
 * @returns {Promise<any>} Response data
 * @throws {Error} If the request fails
 */
async function railwayGraphQL(query, variables) {
  const token = getRequiredEnvVar("RAILWAY_API_TOKEN");

  const response = await fetch("https://backboard.railway.com/graphql/v2", {
    method: "POST",
    headers: {
      Authorization: `Bearer ${token}`,
      "Content-Type": "application/json",
    },
    body: JSON.stringify({ query, variables }),
  });

  if (!response.ok) {
    throw new Error(
      `Railway API request failed with status ${response.status}`,
    );
  }

  const result = await response.json();

  if (result.errors) {
    throw new Error(
      `Railway API returned errors: ${JSON.stringify(result.errors)}`,
    );
  }

  return result.data;
}

/**
 * Gets a required environment variable.
 * @param {string} name - Name of the environment variable
 * @returns {string} Value of the environment variable
 * @throws {Error} If the environment variable is not set
 */
function getRequiredEnvVar(name) {
  const value = process.env[name];
  if (!value) {
    throw new Error(`Required environment variable ${name} is not set`);
  }
  return value;
}

/**
 * Parses command line arguments.
 * @returns {{projectName: string}} Parsed arguments
 * @throws {Error} If arguments are invalid
 */
function parseArgs() {
  const args = process.argv.slice(2);

  if (args.length !== 1) {
    console.error("Usage: node cleanup-railway-project.mjs <project-name>");
    throw new Error("Invalid number of arguments");
  }

  return {
    projectName: args[0],
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
