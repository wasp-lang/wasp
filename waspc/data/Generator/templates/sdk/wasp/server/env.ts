{{={= =}=}}
import * as z from "zod"
import { ensureEnvSchema } from "../env/validation"
import { FromRegister } from "../types/register";
{=# envValidationSchema.isDefined =}
{=& envValidationSchema.importStatement =}
{=/ envValidationSchema.isDefined =}

export type RegisteredServerEnvValidationSchema = FromRegister<"serverEnvValidationSchema", z.ZodObject<{}>>;
type UserServerEnvSchema = RegisteredServerEnvValidationSchema;

{=# envValidationSchema.isDefined =}
const userServerEnvSchema: UserServerEnvSchema = {= envValidationSchema.importIdentifier =};
{=/ envValidationSchema.isDefined =}
{=^ envValidationSchema.isDefined =}
const userServerEnvSchema: UserServerEnvSchema = z.object({});
{=/ envValidationSchema.isDefined =}

const waspCommonServerEnvSchema = z.object({
  PORT: z.coerce.number().default({= defaultServerPort =}),
  {= databaseUrlEnvVarName =}: z.string({
    error: '{= databaseUrlEnvVarName =} is required',
  }),
  PG_BOSS_NEW_OPTIONS: z.string().optional(),
  {=# isEmailSenderEnabled =}
  {=# enabledEmailSenders.isSmtpProviderEnabled =}
  SMTP_HOST: z.string({
    error: getRequiredEnvVarErrorMessage('SMTP email sender', 'SMTP_HOST'),
  }),
  SMTP_PORT: z.coerce.number({
    error: getRequiredEnvVarErrorMessage('SMTP email sender', 'SMTP_PORT'),
  }),
  SMTP_USERNAME: z.string({
    error: getRequiredEnvVarErrorMessage('SMTP email sender', 'SMTP_USERNAME'),
  }),
  SMTP_PASSWORD: z.string({
    error: getRequiredEnvVarErrorMessage('SMTP email sender', 'SMTP_PASSWORD'),
  }),
  {=/ enabledEmailSenders.isSmtpProviderEnabled =}
  {=# enabledEmailSenders.isSendGridProviderEnabled =}
  SENDGRID_API_KEY: z.string({
    error: getRequiredEnvVarErrorMessage('SendGrid email sender', 'SENDGRID_API_KEY'),
  }),
  {=/ enabledEmailSenders.isSendGridProviderEnabled =}
  {=# enabledEmailSenders.isMailgunProviderEnabled =}
  MAILGUN_API_KEY: z.string({
    error: getRequiredEnvVarErrorMessage('Mailgun email sender', 'MAILGUN_API_KEY'),
  }),
  MAILGUN_DOMAIN: z.string({
    error: getRequiredEnvVarErrorMessage('Mailgun email sender', 'MAILGUN_DOMAIN'),
  }),
  MAILGUN_API_URL: z.string().optional(),
  {=/ enabledEmailSenders.isMailgunProviderEnabled =}
  {=# enabledEmailSenders.isResendProviderEnabled =}
  RESEND_API_KEY: z.string({
    error: getRequiredEnvVarErrorMessage('Resend email sender', 'RESEND_API_KEY'),
  }),
  {=/ enabledEmailSenders.isResendProviderEnabled =}
  {=/ isEmailSenderEnabled =}
  {=# isAuthEnabled =}
  {=! Env vars the auth providers' manifests declared. Rendering them
      here means a missing var fails at boot with the manifest's own
      explanation, not at the first authenticated request. Vars with a
      devDefault render in the dev/prod schemas below instead, so the default
      applies only in development. =}
  {=# authProviderServerEnvVars =}
  {=^ hasDevDefault =}
  "{= name =}": z.string({
    error: {=& errorJson =},
  }){=# isOptional =}.optional(){=/ isOptional =},
  {=/ hasDevDefault =}
  {=/ authProviderServerEnvVars =}
  {=/ isAuthEnabled =}
});

const serverUrlSchema =
  z.string({
    error: '{= serverUrlEnvVarName =} is required',
  })
  .pipe(
    z.url({
      error: '{= serverUrlEnvVarName =} must be a valid URL',
    })
  )

const clientUrlSchema =
  z.string({
    error: '{= clientUrlEnvVarName =} is required',
  })
  .pipe(
    z.url({
      error: '{= clientUrlEnvVarName =} must be a valid URL',
    })
  )

// In development, we provide default values for some environment variables
// to make the development process easier.
const waspDevServerEnvSchema = z.object({
  NODE_ENV: z.literal("development"),
  "{= serverUrlEnvVarName =}": serverUrlSchema
    .default("{= defaultServerUrl =}"),
  "{= clientUrlEnvVarName =}": clientUrlSchema
    .default("{= defaultClientUrl =}"),
  {=# isAuthEnabled =}
  {=# authProviderServerEnvVars =}
  {=# hasDevDefault =}
  "{= name =}": z.string({
    error: {=& errorJson =},
  }).default({=& devDefaultJson =}),
  {=/ hasDevDefault =}
  {=/ authProviderServerEnvVars =}
  {=/ isAuthEnabled =}
});

const waspProdServerEnvSchema = z.object({
  NODE_ENV: z.literal("production"),
  "{= serverUrlEnvVarName =}": serverUrlSchema,
  "{= clientUrlEnvVarName =}": clientUrlSchema,
  {=# isAuthEnabled =}
  {=# authProviderServerEnvVars =}
  {=# hasDevDefault =}
  "{= name =}": z.string({
    error: {=& errorJson =},
  }){=# isOptional =}.optional(){=/ isOptional =},
  {=/ hasDevDefault =}
  {=/ authProviderServerEnvVars =}
  {=/ isAuthEnabled =}
});

const waspServerEnvSchema = z.discriminatedUnion("NODE_ENV", [
  z.object({...waspCommonServerEnvSchema.shape, ...waspDevServerEnvSchema.shape}),
  z.object({...waspCommonServerEnvSchema.shape, ...waspProdServerEnvSchema.shape}),
]);

type CompleteServerEnvSchema = z.ZodIntersection<UserServerEnvSchema, typeof waspServerEnvSchema>;

const serverEnvSchema: CompleteServerEnvSchema = userServerEnvSchema.and(waspServerEnvSchema);

const defaultNodeEnvValue = waspDevServerEnvSchema.shape.NODE_ENV.value;
const { NODE_ENV: inputNodeEnvValue, ...restEnv } = process.env;

// PUBLIC API
export const env: z.infer<CompleteServerEnvSchema> = ensureEnvSchema(
  {
    NODE_ENV: inputNodeEnvValue ?? defaultNodeEnvValue,
    ...restEnv,
  },
  serverEnvSchema,
);

function getRequiredEnvVarErrorMessage(featureName: string, envVarName: string) {
  return `${envVarName} is required when using ${featureName}`
}
