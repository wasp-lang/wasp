import type { Config } from "@react-router/dev/config";

let isSSR = false;

{=# ssr.isDefined =}
 isSSR = Boolean("{= ssr.value =}")
{=/ ssr.isDefined =}, 

export default {
  appDirectory: "src",
  ssr: isSSR,
} satisfies Config;
