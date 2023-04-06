import { createTheme } from '@stitches/react'

export enum State {
    Login = "login",
    Signup = "signup",
    ForgotPassword = "forgot-password",
    ResetPassword = "reset-password",
    VerifyEmail = "verify-email",
}

export type CustomizationOptions = {
    logo?: string; 
    socialLayout?: "horizontal" | "vertical";
    appearance?: Parameters<typeof createTheme>[0];
}
