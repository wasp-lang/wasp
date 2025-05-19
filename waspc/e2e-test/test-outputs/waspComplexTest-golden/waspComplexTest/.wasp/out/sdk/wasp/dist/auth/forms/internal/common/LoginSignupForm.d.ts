import type { AdditionalSignupFields } from '../../types';
export type LoginSignupFormFields = {
    [key: string]: string;
};
export declare const LoginSignupForm: ({ state, socialButtonsDirection, additionalSignupFields, }: {
    state: "login" | "signup";
    socialButtonsDirection?: "horizontal" | "vertical";
    additionalSignupFields?: AdditionalSignupFields;
}) => import("react").JSX.Element;
//# sourceMappingURL=LoginSignupForm.d.ts.map