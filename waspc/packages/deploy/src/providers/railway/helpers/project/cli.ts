import { exit } from 'process';

import { $ } from 'zx';

import { DeploymentInfo } from '../../DeploymentInfo.js';
import { SetupOptions } from '../../setup/SetupOptions.js';
import { waspSays } from '../../../../helpers.js';

export async function initProject({
  baseName,
  options,
}: DeploymentInfo<SetupOptions>): Promise<RailwayProject> {
  await $({
    // If there are multiple workspaces, the user needs to select **interactively**
    // which one to use. We need to allow users to select the workspace interactively.
    // There is no way to pass it as a command line argument.
    stdio: 'inherit',
  })`${options.railwayExe} init --name ${baseName}`;

  // Check if the project was created successfully...
  const newProject = await getProjectForCurrentDir(options.railwayExe);
  if (newProject === null) {
    waspSays('Project creation failed. Exiting...');
    exit(1);
  } else {
    waspSays('Project created successfully!');
  }

  return new RailwayProject(newProject);
}

export async function linkProject(
  { options }: DeploymentInfo<SetupOptions>,
  project: RailwayProject,
): Promise<RailwayProject> {
  const serviceArg = project.services.length > 0 ? ['-s', project.services[0].name] : [];

  await $`${options.railwayExe} link -p ${project.name} ${serviceArg}`;

  // Check if the project was linked successfully...
  const linkedProject = await getProjectForCurrentDir(options.railwayExe);
  if (linkedProject === null) {
    waspSays('Project linking failed. Exiting...');
    exit(1);
  } else {
    waspSays('Project linked successfully!');
  }

  return new RailwayProject(linkedProject);
}

export async function getProjectForCurrentDir(railwayExe: string): Promise<RailwayProject | null> {
  const result = await $({
    verbose: false,
    nothrow: true,
    // Ignoring stdin and stderr to stop error output from Railway CLI
    stdio: ['ignore', 'pipe', 'ignore'],
  })`${railwayExe} status --json`;
  if (result.exitCode !== 0) {
    return null;
  } else {
    const json = JSON.parse(result.stdout) as unknown;

    return mapProject(json);
  }
}

export async function getProjects(railwayExe: string) {
  const result = await $({
    verbose: false,
  })`${railwayExe} list --json`;

  const json = JSON.parse(result.stdout) as unknown[];

  return json.map((rawProject) => {
    return mapProject(rawProject);
  });
}

// TODO: use runtime validation to avoid using `any`
function mapProject(json: any): RailwayProject {
  const services = json.services.edges.map((edge: any) => ({
    id: edge.node.id,
    name: edge.node.name,
  }));
  return new RailwayProject({
    id: json.id,
    name: json.name,
    services,
  });
}

export class RailwayProject {
  id: string;
  name: string;
  services: RailwayService[];

  constructor({ id, name, services }: { id: string; name: string; services: RailwayService[] }) {
    this.id = id;
    this.name = name;
    this.services = services;
  }

  doesServiceExist(serviceName: string): boolean {
    return this.services.map((s) => s.name).includes(serviceName);
  }
}

export type RailwayService = {
  id: string;
  name: string;
};
