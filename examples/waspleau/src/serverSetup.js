import { github, loadTime } from "wasp/server/jobs";

export default async function () {
  await github.submit();
  await loadTime.submit({
    url: "https://wasp.sh",
    name: "wasp.sh Load Time",
  });
}
