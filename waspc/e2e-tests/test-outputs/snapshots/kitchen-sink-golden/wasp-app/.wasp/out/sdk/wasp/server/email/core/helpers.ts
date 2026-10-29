import type { EmailFromField } from '@wasp/lib-sdk-core/node'

// PRIVATE API
export function getDefaultFromField(): EmailFromField {
  return {
    email: "kitchen-sink@wasp.sh",
  }
}
