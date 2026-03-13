import { useState } from "react";
import {
  FormError,
  FormInput,
  FormItemGroup,
  FormLabel,
} from "wasp/client/auth";
import { customSignup as customSubmit } from "wasp/client/operations";
// Missing SubmitButton export
// import { SubmitButton } from 'wasp/client/auth'
import { useForm } from "react-hook-form";
import { Alert } from "../../../components/Alert";
import { Button } from "../../../components/Button";
import { FeatureContainer } from "../../../components/FeatureContainer";

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
          <FormItemGroup>
            <FormLabel>E-mail</FormLabel>
            <FormInput type="email" {...register("email")} />
            <FormError>{errors.email?.message}</FormError>
          </FormItemGroup>
          <FormItemGroup>
            <FormLabel>Password</FormLabel>
            <FormInput type="password" {...register("password")} />
            <FormError>{errors.password?.message}</FormError>
          </FormItemGroup>
          <FormItemGroup>
            <FormLabel>Address</FormLabel>
            <FormInput {...register("address")} />
            <FormError>{errors.address?.message}</FormError>
          </FormItemGroup>
          <FormItemGroup>
            <Button type="submit">Sign up</Button>
          </FormItemGroup>
        </form>
      </div>
    </FeatureContainer>
  );
};
