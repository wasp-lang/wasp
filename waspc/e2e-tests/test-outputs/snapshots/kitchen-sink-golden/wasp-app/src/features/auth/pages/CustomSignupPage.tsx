import { useState, type PropsWithChildren } from "react";
import { customSignup as customSubmit } from "wasp/client/operations";
import { useForm } from "react-hook-form";
import { Alert } from "../../../components/Alert";
import { Button } from "../../../components/Button";
import { FeatureContainer } from "../../../components/FeatureContainer";
import { Input } from "../../../components/Input";

export const CustomSignupPage = () => {
  const {
    register,
    handleSubmit,
    formState: { errors },
  } = useForm<{
    email: string;
    password: string;
    address: string;
  }>();
  const [message, setMessage] = useState<{
    type: "success" | "error";
    text: string;
  } | null>(null);

  const onSubmit = handleSubmit(async (data) => {
    try {
      const result = await customSubmit(data);
      console.error("result", result);
      if (result.success) {
        setMessage({
          type: "success",
          text: "Signup successful. You can now login.",
        });
      } else {
        setMessage({
          type: "error",
          text: result.message,
        });
      }
    } catch (error: any) {
      const { message, data } = error.data;
      setMessage({
        type: "error",
        text: `${message}: ${data.message}`,
      });
    }
  });

  return (
    <FeatureContainer>
      <div className="space-y-4">
        <h2 className="feature-title">Custom Signup Form</h2>
        <form
          onSubmit={onSubmit}
          className="card"
          data-testid="custom-signup-form"
        >
          {message && (
            <Alert variant={message.type} className="mb-4">
              <span data-testid="message">{message.text}</span>
            </Alert>
          )}
          <FieldError message={errors.email?.message}>
            <Input type="email" label="E-mail" {...register("email")} />
          </FieldError>
          <FieldError message={errors.password?.message}>
            <Input type="password" label="Password" {...register("password")} />
          </FieldError>
          <FieldError message={errors.address?.message}>
            <Input label="Address" {...register("address")} />
          </FieldError>
          <div className="space-y-1">
            <Button type="submit">Sign up</Button>
          </div>
        </form>
      </div>
    </FeatureContainer>
  );
};

function FieldError({ children, message }: PropsWithChildren<{ message?: string }>) {
  return (
    <div className="space-y-1">
      {children}
      {message && <p className="text-sm font-medium text-red-600">{message}</p>}
    </div>
  );
}
