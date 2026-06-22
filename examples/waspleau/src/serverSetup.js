import { measureLoadTime, refreshGitHubRepoData } from "wasp/server/jobs";

export default async function () {
  await refreshGitHubRepoData.submit();
  await measureLoadTime.submit({
    url: "https://wasp.sh",
    name: "wasp.sh Load Time",
  });
}
