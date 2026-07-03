import { fs, path } from "zx";

/**
 * Configures the Wasp for MailCrab SMTP email server which is used by `wasp-app-runner`.
 * Assumes the Wasp project uses `main.wasp.ts` or `main.wasp.tsx` as the Wasp spec file.
 */
export async function setupWaspMailCrabConfiguration(
  waspProjectPath: string,
): Promise<void> {
  const waspServerEnvFilePath = path.join(waspProjectPath, ".env.server");
  await fs.ensureFile(waspServerEnvFilePath);
  setupMailCrabEnvVariables(waspServerEnvFilePath);

  const waspAppSpecPath = await findWaspAppSpecPath(waspProjectPath);
  setupMailCrabWaspTsSpec(waspAppSpecPath);
}

async function findWaspAppSpecPath(waspProjectPath: string): Promise<string> {
  const candidatePaths = ["main.wasp.tsx", "main.wasp.ts"].map((fileName) =>
    path.join(waspProjectPath, fileName),
  );
  for (const candidatePath of candidatePaths) {
    if (await fs.pathExists(candidatePath)) {
      return candidatePath;
    }
  }
  throw new Error(`No Wasp spec file found in ${waspProjectPath}`);
}

async function setupMailCrabEnvVariables(
  waspServerEnvFilePath: string,
): Promise<void> {
  const mailCrabSMTPEnvVars = `
SMTP_HOST=localhost
SMTP_USERNAME=any
SMTP_PASSWORD=any
SMTP_PORT=1025
`;

  await fs.appendFile(waspServerEnvFilePath, mailCrabSMTPEnvVars);
}

async function setupMailCrabWaspTsSpec(waspTsSpecPath: string): Promise<void> {
  const waspTsSpec = await fs.readFile(waspTsSpecPath, "utf8");
  const waspSMTPAppSpec = waspTsSpec.replace(
    /provider:\s+[A-Za-z0-9_][A-Za-z0-9_]*/g,
    "provider: SMTP",
  );

  await fs.writeFile(waspTsSpecPath, waspSMTPAppSpec);
}
