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
import { Button } from "../../../components/Button";
import { SimplePageContainer } from "../../../components/SimplePageContainer";

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
    <SimplePageContainer>
      <h1 className="text-2xl font-medium mb-4">Custom Signup Form</h1>
      <form onSubmit={onSubmit} className="card">
        {message && (
          <div
            style={{
              color: message.type === "error" ? "red" : "green",
              marginBottom: "1rem",
            }}
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
    </SimplePageContainer>
  );
};
