import "./internal/auth-styles.css";
import { State, type AdditionalSignupFields, type CustomizationOptions } from "./types.js";
export declare function Auth({ state, appearance, logo, socialLayout, additionalSignupFields, }: {
    state: State;
} & CustomizationOptions & {
    additionalSignupFields?: AdditionalSignupFields;
}): import("react").JSX.Element;
