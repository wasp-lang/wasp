import toml from 'toml'
import fs from 'fs'
import path from 'node:path'
import { IGlobalOptions } from '../IGlobalOptions.js'

export interface ITomlFilePaths {
  serverTomlPath: string
  clientTomlPath: string
}

export function getTomlFileInfo(options: IGlobalOptions): ITomlFilePaths {
  const baseDir = options.tomlDir || options.waspDir
  return {
    serverTomlPath: path.join(baseDir, 'fly-server.toml'),
    clientTomlPath: path.join(baseDir, 'fly-client.toml')
  }
}

export function serverTomlExists(paths: ITomlFilePaths): boolean {
  return fs.existsSync(paths.serverTomlPath)
}

export function copyLocalTomlAsServerToml(paths: ITomlFilePaths) {
  fs.copyFileSync('fly.toml', paths.serverTomlPath)
}

export function copyServerTomlLocally(paths: ITomlFilePaths) {
  fs.copyFileSync(paths.serverTomlPath, 'fly.toml')
}

export function clientTomlExists(paths: ITomlFilePaths): boolean {
  return fs.existsSync(paths.clientTomlPath)
}

export function copyLocalTomlAsClientToml(paths: ITomlFilePaths) {
  fs.copyFileSync('fly.toml', paths.clientTomlPath)
}

export function copyClientTomlLocally(paths: ITomlFilePaths) {
  fs.copyFileSync(paths.clientTomlPath, 'fly.toml')
}

export function localTomlExists(): boolean {
  return fs.existsSync('fly.toml')
}

export function deleteLocalToml() {
  if (localTomlExists()) {
    fs.unlinkSync('fly.toml')
  }
}

export function getAppNameFromToml(path: string): string {
  const content = fs.readFileSync(path, 'utf8')
  const data = toml.parse(content)
  return data.app
}

export function replaceLineInLocalToml(searchValue: string | RegExp, replaceValue: string) {
  const content = fs.readFileSync('fly.toml', 'utf8')
  const updatedContent = content.replace(searchValue, replaceValue)
  fs.writeFileSync('fly.toml', updatedContent)
}
