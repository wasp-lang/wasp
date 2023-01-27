import toml from 'toml';
import fs from 'fs';
import path from 'node:path';
import { GlobalOptions } from '../GlobalOptions.js';

export interface TomlFilePaths {
	serverTomlPath: string;
	clientTomlPath: string;
}

export function getTomlFilePaths(options: GlobalOptions): TomlFilePaths {
	const baseDir = options.tomlDir || options.waspDir;
	return {
		serverTomlPath: path.join(baseDir, 'fly-server.toml'),
		clientTomlPath: path.join(baseDir, 'fly-client.toml'),
	};
}

export function serverTomlExistsInProject(paths: TomlFilePaths): boolean {
	return fs.existsSync(paths.serverTomlPath);
}

export function copyLocalServerTomlToProject(paths: TomlFilePaths): void {
	fs.copyFileSync('fly.toml', paths.serverTomlPath);
}

export function copyProjectServerTomlLocally(paths: TomlFilePaths): void {
	fs.copyFileSync(paths.serverTomlPath, 'fly.toml');
}

export function clientTomlExistsInProject(paths: TomlFilePaths): boolean {
	return fs.existsSync(paths.clientTomlPath);
}

export function copyLocalClientTomlToProject(paths: TomlFilePaths): void {
	fs.copyFileSync('fly.toml', paths.clientTomlPath);
}

export function copyProjectClientTomlLocally(paths: TomlFilePaths): void {
	fs.copyFileSync(paths.clientTomlPath, 'fly.toml');
}

export function localTomlExists(): boolean {
	return fs.existsSync('fly.toml');
}

export function deleteLocalToml(): void {
	if (localTomlExists()) {
		fs.unlinkSync('fly.toml');
	}
}

export function getAppNameFromToml(path: string): string {
	const content = fs.readFileSync(path, 'utf8');
	const data = toml.parse(content);
	return data.app;
}

export function getInferredBasenameFromServerToml(paths: TomlFilePaths): string {
	const serverName = getAppNameFromToml(paths.serverTomlPath);
	return serverName.replace('-server', '');
}

export function getInferredBasenameFromClientToml(paths: TomlFilePaths): string {
	const clientName = getAppNameFromToml(paths.clientTomlPath);
	return clientName.replace('-client', '');
}

export function replaceLineInLocalToml(searchValue: string | RegExp, replaceValue: string): void {
	const content = fs.readFileSync('fly.toml', 'utf8');
	const updatedContent = content.replace(searchValue, replaceValue);
	fs.writeFileSync('fly.toml', updatedContent);
}
