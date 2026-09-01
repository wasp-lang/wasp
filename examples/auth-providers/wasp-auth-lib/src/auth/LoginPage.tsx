import { AuthForm } from "@wasp.sh/auth/client";

export function LoginPage() {
  return <AuthForm onSuccess={() => (window.location.href = "/")} />;
}
