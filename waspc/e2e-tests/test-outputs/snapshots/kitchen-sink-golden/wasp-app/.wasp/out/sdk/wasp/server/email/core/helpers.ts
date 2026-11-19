import type { EmailFromField } from '@wasp/sdk-core/node'

// PRIVATE API
export function getDefaultFromField(): EmailFromField {
  return {
    email: "kitchen-sink@wasp.sh",
  }
}
