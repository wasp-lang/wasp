import { github, loadTime } from "wasp/server/jobs";

export default async function () {
  await github.submit()
  await loadTime.submit({
    url: "https://wasp-lang.dev",
    name: "wasp-lang.dev Load Time"
  })
}
