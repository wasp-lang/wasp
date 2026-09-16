import "./custom-register";
type DecimalClass = typeof import("@prisma/client/runtime/library").Decimal;
type DecimalInstance = InstanceType<DecimalClass>;
declare module "superjson" {
    interface WaspInternal_CustomSerializableJSONValue_Register {
        Decimal: DecimalInstance;
    }
}
export {};
//# sourceMappingURL=prisma.d.ts.map