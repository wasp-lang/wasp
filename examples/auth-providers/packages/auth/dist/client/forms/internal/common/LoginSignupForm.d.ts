import type { AdditionalSignupFields } from "../../types.js";
import "../auth-styles.css";
export declare const LoginSignupForm: ({ state, socialButtonsDirection, additionalSignupFields, }: {
    state: "login" | "signup";
    socialButtonsDirection?: "horizontal" | "vertical";
    additionalSignupFields?: AdditionalSignupFields;
}) => import("react").JSX.Element;
