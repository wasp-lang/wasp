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
import { cn } from "../../../cn";
import { Button } from "../../../components/Button";
import { FeatureContainer } from "../../../components/FeatureContainer";

export const SignupPage = () => {
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
      <h1 className="text-2xl font-medium mb-4">Custom Signup Form</h1>
      <form
        onSubmit={onSubmit}
        className="card"
        data-testid="custom-signup-form"
      >
        {message && (
          <div
            className={cn(
              "mb-4 p-4 rounded-lg",
              message.type === "error" &&
                "bg-red-50 border border-red-200 text-red-600",
              message.type === "success" &&
                "bg-green-50 border border-green-200 text-green-600",
            )}
            data-testid="message"
          >
            {message.text}
          </div>
        )}
        <FormItemGroup>
          <FormLabel>Email</FormLabel>
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
          <Button type="submit">Signup</Button>
        </FormItemGroup>
      </form>
    </FeatureContainer>
  );
};
