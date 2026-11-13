import type { ReactNode } from "react";
import { Link as RouterLink } from "react-router";
import {
  LoginForm,
  SignupForm,
  type CustomizationOptions,
} from "wasp/client/auth";
import { Card, CardContent } from "../components/ui/card";

// Wasp's auth forms, wrapped in shadcn cards and themed with the same tokens.
const appearance: CustomizationOptions["appearance"] = {
  colors: {
    brand: "var(--primary)",
    brandAccent: "var(--primary)",
    submitButtonText: "var(--primary-foreground)",
  },
};

function AuthCard({
  children,
  footer,
}: {
  children: ReactNode;
  footer?: ReactNode;
}) {
  return (
    <div className="flex min-h-[70vh] items-center justify-center">
      <div className="w-full max-w-sm space-y-4">
        <Card>
          <CardContent>{children}</CardContent>
        </Card>
        {footer && (
          <div className="text-muted-foreground text-center text-sm">
            {footer}
          </div>
        )}
      </div>
    </div>
  );
}

const linkClass = "font-medium text-foreground underline underline-offset-4";

export function LoginPage() {
  return (
    <AuthCard
      footer={
        <p>
          No account yet?{" "}
          <RouterLink className={linkClass} to="/signup">
            Sign up
          </RouterLink>
        </p>
      }
    >
      <LoginForm appearance={appearance} />
    </AuthCard>
  );
}

export function SignupPage() {
  return (
    <AuthCard
      footer={
        <p>
          Already have an account?{" "}
          <RouterLink className={linkClass} to="/login">
            Log in
          </RouterLink>
        </p>
      }
    >
      <SignupForm appearance={appearance} />
    </AuthCard>
  );
}
