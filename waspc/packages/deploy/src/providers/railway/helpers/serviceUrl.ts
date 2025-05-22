import { exit } from 'process';
import { $ } from 'zx';

import { waspSays } from '../../../helpers.js';
import { RailwayCliDomainSchema } from '../schemas.js';

export enum ServiceUrlStatus {
  URL_CREATED = 'URL_CREATED',
  URL_ALREADY_EXISTS = 'URL_ALREADY_EXISTS',
  UNKNOWN_ERROR = 'UNKNOWN_ERROR',
}

export async function getServiceUrl(
  railwayExe: string,
  serviceName: string,
  port: number,
): Promise<string> {
  const { status, output } = await getServiceUrlStatus(railwayExe, serviceName, port);

  switch (status) {
    case ServiceUrlStatus.URL_CREATED:
      const { domain } = RailwayCliDomainSchema.parse(JSON.parse(output));
      return domain;
    case ServiceUrlStatus.URL_ALREADY_EXISTS:
      // NOTE: Dealing with Railway CLI output quirk that if the domain already exists,
      // the output is not in JSON format, but rather a string with the domain URL.
      return matchServiceUrl(output);
    case ServiceUrlStatus.UNKNOWN_ERROR:
      waspSays(`There was a problem getting a domain for ${serviceName}.`);
      exit(1);
    default:
      status satisfies never;
      throw new Error(`Unhandled status: ${status}`);
  }
}

async function getServiceUrlStatus(railwayExe: string, serviceName: string, port: number) {
  const result = await $`${railwayExe} domain --service "${serviceName}" --port ${port} --json`;

  if (result.exitCode !== 0) {
    return {
      status: ServiceUrlStatus.UNKNOWN_ERROR,
    } as const;
  }

  if (result.stdout.includes('Domains already exists on the service:')) {
    return {
      status: ServiceUrlStatus.URL_ALREADY_EXISTS,
      output: result.stdout,
    } as const;
  } else {
    return {
      status: ServiceUrlStatus.URL_CREATED,
      output: result.stdout,
    } as const;
  }
}

function matchServiceUrl(text: string): string {
  const match = text.match(/https:\/\/[^\s]*/);
  if (match === null) {
    throw new Error('Failed to get service domain');
  }
  return match[0];
}
