import { Register } from "../../types/register";

export type OperationFromRegister<
  Operation extends string,
  Fallback,
  Subregister = "operations",
> = Subregister extends keyof Register
  ? Operation extends keyof Register[Subregister]
    ? Register[Subregister][Operation]
    : Fallback
  : Fallback;
