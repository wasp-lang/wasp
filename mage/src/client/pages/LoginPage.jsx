import { LoginForm } from "@wasp/auth/forms/Login";
import { Header } from "../components/Header";

export function LoginPage() {
  return (
    <>
      <Header />
      <div className="big-box">
        <LoginForm />
      </div>
    </>
  );
}
