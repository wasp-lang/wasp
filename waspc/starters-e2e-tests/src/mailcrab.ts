import { fs, path } from "zx";

/**
 * Configures the Wasp for MailCrab SMTP email server which is used by `wasp-app-runner`.
 * Assumes the Wasp project uses the `main.wasp.ts` as the Wasp spec file.
 */
export async function setupWaspMailCrabConfiguration(
  waspProjectPath: string,
): Promise<void> {
  const waspServerEnvFilePath = path.join(waspProjectPath, ".env.server");
  await fs.ensureFile(waspServerEnvFilePath);
  setupMailCrabEnvVariables(waspServerEnvFilePath);

  const waspAppSpecPath = path.join(waspProjectPath, "main.wasp.ts");
  setupMailCrabWaspTsSpec(waspAppSpecPath);
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
  // NOTE: only email sender provider names may match here (SMTP itself needs no
  // rewrite). `app.auth` also has a `provider:` field now (e.g.
  // `provider: waspAuth({...})`), and a broader pattern would rewrite it into
  // `provider: SMTP({...})` and corrupt the spec.
  const waspSMTPAppSpec = waspTsSpec.replace(
    /provider:\s+(SendGrid|Mailgun|Resend|Dummy)\b/g,
    "provider: SMTP",
  );

  await fs.writeFile(waspTsSpecPath, waspSMTPAppSpec);
}
