import { fs, path } from "zx";

/**
 * Configures the Wasp for MailCrab SMTP email server which is used by `wasp-app-runner`.
 *
 * Assumes the Wasp project uses the `main.wasp` as the Wasp configuration file.
 */
export async function setupWaspMailCrabConfiguration(
  waspProjectPath: string,
): Promise<void> {
  const waspServerEnvFilePath = path.join(waspProjectPath, ".env.server");
  await fs.ensureFile(waspServerEnvFilePath);
  setupMailCrabEnvVariables(waspServerEnvFilePath);

  const waspAppSpecPath = path.join(waspProjectPath, "main.wasp");
  setupMailCrabWaspAppSpec(waspAppSpecPath);
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

async function setupMailCrabWaspAppSpec(
  waspAppSpecPath: string,
): Promise<void> {
  const waspAppSpec = await fs.readFile(waspAppSpecPath, "utf8");
  const waspSMTPAppSpec = waspAppSpec.replace(
    /provider:\s+[A-Za-z0-9_][A-Za-z0-9_]*/g,
    "provider: SMTP",
  );

  await fs.writeFile(waspAppSpecPath, waspSMTPAppSpec);
}
