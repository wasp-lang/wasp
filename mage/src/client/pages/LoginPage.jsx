import { LoginForm } from "wasp/client/auth";

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
