import {
  FormItemGroup,
  FormLabel,
  FormInput,
  FormError,
} from "wasp/client/auth";
import { useForm } from "react-hook-form";
import { migratePassword } from "wasp/client/operations";
import { useState } from "react";

export function MigratePasswordPage() {
  const [successMessage, setSuccessMessage] = useState<string | null>(null);
  const [errorMessage, setErrorMessage] = useState<string | null>(null);
  const form = useForm<{
    username: string;
    password: string;
  }>();

  const onSubmit = form.handleSubmit(async (data) => {
    try {
      const result = await migratePassword(data);
      setSuccessMessage(result.message);
    } catch (e: unknown) {
      console.error(e);
      if (e instanceof Error) {
        setErrorMessage(e.message);
      }
    }
  });

  return (
    <div style={{
      maxWidth: "400px",
      margin: "auto",
    }}>
      <h1>Migrate your password</h1>
      <p>
        If you have an account on the old version of the website, you can
        migrate your password to the new version.
      </p>
      {successMessage && <div>{successMessage}</div>}
      {errorMessage && <FormError>{errorMessage}</FormError>}
      <form onSubmit={onSubmit}>
        <FormItemGroup>
          <FormLabel>Username</FormLabel>
          <FormInput
            {...form.register("username", {
              required: "Username is required",
            })}
          />
          <FormError>{form.formState.errors.username?.message}</FormError>
        </FormItemGroup>
        <FormItemGroup>
          <FormLabel>Password</FormLabel>
          <FormInput
            {...form.register("password", {
              required: "Password is required",
            })}
            type="password"
          />
          <FormError>{form.formState.errors.password?.message}</FormError>
        </FormItemGroup>
        <button type="submit">Migrate password</button>
      </form>
    </div>
  );
}