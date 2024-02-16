/// <reference types="react" />
import { type CustomizationOptions, type AdditionalSignupFields } from './types';
export declare function SignupForm({ appearance, logo, socialLayout, additionalFields, }: CustomizationOptions & {
    additionalFields?: AdditionalSignupFields;
}): import("react").JSX.Element;
