import fs from 'fs';
import path from 'path';
import { Plugin } from 'vite';
import crypto from 'crypto';

interface DevToolsJSON {
    workspace?: { root: string, uuid: string };
}

const ENDPOINT = '/.well-known/appspecific/com.chrome.devtools.json';

export default function devToolsJsonPlugin(): Plugin {
    return {
        name: 'devtools-json',
        enforce: 'post',
        configureServer(server) {
            const { config, middlewares } = server;
            const { logger } = config;
            if (!config.env.DEV) return;

            const getOrCreateUUID = (): string => {
                let { cacheDir } = config;
                let { root } = config;
                if (!path.isAbsolute(root))
                    root = path.resolve(process.cwd(), root);

                if (!path.isAbsolute(cacheDir)) {
                    cacheDir = path.resolve(root, cacheDir);

                }
                const projectHash = getProjectHash(root);
                const uuidDir = path.resolve(cacheDir, projectHash);
                const uuidPath = path.resolve(uuidDir, 'uuid.json');
                const validate = (u: string) =>
                    /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i.test(u);

                try {
                    if (fs.existsSync(uuidPath)) {
                        const cached = fs.readFileSync(uuidPath, { encoding: "utf-8" });
                        if (validate(cached)) {
                            return cached;
                        }
                        logger.warn(
                            "[devtools-json] Cached UUID was invalid – regenerating"
                        );
                  }
                } catch (err) {
                    logger.error(
                        `[devtools-json] Failed to read cached UUID: ${String(err)}`
                    );
                }

                if (!fs.existsSync(uuidDir)) {
                    try {
                        fs.mkdirSync(uuidDir, { recursive: true });
                    } catch (err) {
                        logger.error(
                            `[devtools-json] Failed creating cache dir: ${String(err)}`
                        );
                    }
                }
                const uuid = crypto.randomUUID();
                try {
                    fs.writeFileSync(uuidPath, uuid, { encoding: "utf-8" });
                    logger.info(
                        `[devtools-json] Generated UUID '${uuid}' for DevTools project settings.`
                    );
                } catch (err) {
                    logger.error(
                        `[devtools-json] Unable to persist UUID cache: ${String(err)}`
                    );
                }
                return uuid;
            }
            const getProjectHash = (root: string): string => {
                return crypto.createHash('sha1').update(root).digest('hex').slice(0, 8);
            }
            const normalizeForChrome = (root: string): string => {
                if (path.isAbsolute(root) && root[1] === ':')
                    return root;
                if (process.env.WSL_DISTRO_NAME) {
                    const distroName = process.env.WSL_DISTRO_NAME;
                    const withoutLeadingSlash = root.replace(/^\//, '');
                    root = path
                        .join('\\\\wsl.localhost', distroName, withoutLeadingSlash)
                        .replace(/\//g, '\\');
                    logger.info('[devtools-json] Path rewritten for WSL');
                }
                // if we use cgroup , it will work for linux docker containers, but not for the main host which is running docker
                //need to think about it 
                if (process.env.DOCKER_DESKTOP && !root.startsWith('\\\\')) {
                    const withoutLeadingSlash = root.replace(/^\//, '');
                    root = path.join('\\\\wsl.localhost', 'docker-desktop-data', withoutLeadingSlash)
                        .replace(/\//g, '\\');
                    logger.info('[devtools-json] Path rewritten for Docker Desktop');
                }
                return root;
            }
            const findProjectRoot = (): string => {
                // Prefer explicit env var if provided.
                const envRoot = process.env.WASP_PROJECT_ROOT;
                if (envRoot && envRoot.length > 0) {
                    
                    if (!envRoot.includes(`${path.sep}.wasp${path.sep}out`)) {
                        return path.resolve(envRoot);
                    }
                }

               
                let dir = path.resolve(config.root);
                const fsRoot = path.parse(dir).root;
                while (dir !== fsRoot) {
                    if (fs.existsSync(path.join(dir, '.wasp'))) {
                        return dir; // directory that owns the .wasp folder
                    }
                    dir = path.dirname(dir);
                }
                // Could not find `.wasp`, use vite root.
                return path.resolve(config.root);
            };
            middlewares.use(ENDPOINT, (_req, res) => {
                let root = findProjectRoot();
                root = normalizeForChrome(root);

                const uuid = getOrCreateUUID();
                const devtoolsJson: DevToolsJSON = { workspace: { root, uuid } };
                res.setHeader('Content-Type', 'application/json');
                res.end(JSON.stringify(devtoolsJson, null, 2));
            })
        }
    }
}