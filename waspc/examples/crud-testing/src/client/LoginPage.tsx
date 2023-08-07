import { LoginForm } from "@wasp/auth/forms/Login";

export const LoginPage = () => {
  return (
    <div className="container">
      <main>
        <h1>Login</h1>
        <LoginForm />
      </main>
    </div>
  );
};
