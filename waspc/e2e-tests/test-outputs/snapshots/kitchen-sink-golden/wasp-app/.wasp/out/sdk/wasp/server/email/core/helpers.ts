export { formatFromField } from '@wasp.sh/lib-sdk-core/node'

import type { EmailFromField } from "./types";

// PRIVATE API
export function getDefaultFromField(): EmailFromField {
  return {
    email: "kitchen-sink@wasp.sh",
  }
}
