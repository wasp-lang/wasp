import { type State, type CustomizationOptions, type ErrorMessage, type AdditionalSignupFields } from './types';
export declare const AuthContext: import("react").Context<{
    isLoading: boolean;
    setIsLoading: (isLoading: boolean) => void;
    setErrorMessage: (errorMessage: ErrorMessage | null) => void;
    setSuccessMessage: (successMessage: string | null) => void;
}>;
declare function Auth({ state, appearance, logo, socialLayout, additionalSignupFields }: {
    state: State;
} & CustomizationOptions & {
    additionalSignupFields?: AdditionalSignupFields;
}): import("react").JSX.Element;
export default Auth;
//# sourceMappingURL=Auth.d.ts.map