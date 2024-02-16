/// <reference types="react" />
import { createTheme } from '@stitches/react';
import { UseFormReturn, RegisterOptions } from 'react-hook-form';
import type { LoginSignupFormFields } from './internal/common/LoginSignupForm';
export declare enum State {
    Login = "login",
    Signup = "signup"
}
export type CustomizationOptions = {
    logo?: string;
    socialLayout?: 'horizontal' | 'vertical';
    appearance?: Parameters<typeof createTheme>[0];
};
export type ErrorMessage = {
    title: string;
    description?: string;
};
export type FormState = {
    isLoading: boolean;
};
export type AdditionalSignupFieldRenderFn = (hookForm: UseFormReturn<LoginSignupFormFields>, formState: FormState) => React.ReactNode;
export type AdditionalSignupField = {
    name: string;
    label: string;
    type: 'input' | 'textarea';
    validations?: RegisterOptions<LoginSignupFormFields>;
};
export type AdditionalSignupFields = (AdditionalSignupField | AdditionalSignupFieldRenderFn)[] | AdditionalSignupFieldRenderFn;
