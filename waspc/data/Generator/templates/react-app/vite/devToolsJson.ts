import fs from 'fs';
import path from 'path';
import {v4,validate} from 'uuid';
import {Plugin} from 'vite';

interface DevToolsJSON {
    workspace?: {root:string,uuid:string};
}

const ENDPOINT = '/.well-known/appspecific/com.chrome.devtools.json';

export default function devToolsJsonPlugin():Plugin {8
    return {
        name: 'devtools-json',
        enforce: 'post',
        configureServer(server){
            const {config,middlewares} = server;
            const {logger} = config;
            if(!config.env.DEV) return;

            const getOrCreateUUID = ():string => {
                let {cacheDir} = config;
                if (!path.isAbsolute(cacheDir)) {
                    let {root} = config;
                    if (!path.isAbsolute(root)) {
                      root = path.resolve(process.cwd(), root);
                    }
                    cacheDir = path.resolve(root, cacheDir);
                }
                const uuidPath = path.resolve(cacheDir,'uuid.json');
                if(fs.existsSync(uuidPath)){
                  const uuid = fs.readFileSync(uuidPath,{encoding:'utf-8'});
                  if(validate(uuid)){
                    return uuid;
                  }
                }
                if(!fs.existsSync(cacheDir)){
                    fs.mkdirSync(cacheDir,{recursive:true});
                }

                const uuid = v4();
                fs.writeFileSync(uuidPath,uuid,{encoding:'utf-8'});
                logger.info(`Generated UUID '${uuid}' for DevTools project settings.`);
                return uuid;


            }
            const normalizeForChrome = (root:string):string => {
                if(path.isAbsolute(root) && root[1] === ':')
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
                if(process.env.DOCKER_DESKTOP && !root.startsWith('\\\\')){
                    const withoutLeadingSlash = root.replace(/^\//, '');
                    root = path.join('\\\\wsl.localhost','docker-desktop-data',withoutLeadingSlash)
                    .replace(/\//g, '\\');
                    logger.info('[devtools-json] Path rewritten for Docker Desktop');
                }
                return root;
            }
            middlewares.use(ENDPOINT,(_req,res)=>{
                let {root} = config;
                if(!path.isAbsolute(root)){
                    root = path.resolve(process.cwd(),root);

                }
                
                root = normalizeForChrome(root);
               

                const uuid = getOrCreateUUID();
                const devtoolsJson: DevToolsJSON = {workspace:{root,uuid}};
                res.setHeader('Content-Type', 'application/json');
                res.end(JSON.stringify(devtoolsJson, null, 2));
            })
        }
    }
}