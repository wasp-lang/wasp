import type { WaspInternal_CustomSerializableJSONValue_Register } from "superjson";
declare module "superjson" {
    interface WaspInternal_CustomSerializableJSONValue_Register {
    }
}
export type CustomSerializableJSONValue = WaspInternal_CustomSerializableJSONValue_Register[keyof WaspInternal_CustomSerializableJSONValue_Register];
//# sourceMappingURL=custom-register.d.ts.map