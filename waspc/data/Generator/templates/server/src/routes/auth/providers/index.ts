// Generate this file using Wasp
// - if local is used, add the local provider to the providers array
// - for each oauth provider init them and add them to the providers array
import fs from "fs";

import { ProviderConfig } from "./types";

// NOTE: openid-client could be useful for implementing oauth providers

// Glob imports
const providers = await importProviders();

async function importProviders(): Promise<ProviderConfig[]> {
  return new Promise((resolve, reject) => {
    fs.readdir("./config", (err, files) => {
      if (err) {
        return reject(err);
      }
      const imports = files.map((file) => import(`./config/${file}`));
      const configs = await Promise.all(imports);

    });
  });
}

// const providers: Provider[] = [
//   {
//     name: ProviderType.local,
//     slug: "local",
//     isEnabled: config.local,
//     setupRouter: () => {},
//   },
//   {
//     name: ProviderType.google,
//     slug: "google",
//     isEnabled: config.google,
//     setupRouter: () => {},
//   },
//   {
//     name: ProviderType.github,
//     slug: "github",
//     isEnabled: config.github,
//     setupRouter: () => {},
//   },
// ];

// Lifecycle hooks: onBeforeCreate, onCreate
// onBeforeCreate -> used to validate password
// onCreate -> ()

/*
  /auth/{name}/{...rest}

  ->

  oAuth routes:
  /auth/{name}/login
  /auth/{name}/callback?token=

  ->

  Local routes:
  /auth/local/login
  /auth/local/signup

  ->

  General routes:
  /auth/logout
  /auth/me
*/
