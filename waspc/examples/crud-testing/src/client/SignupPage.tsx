import { SignupForm } from "@wasp/auth/forms/Signup";
import {
  FormError,
  FormInput,
  FormItemGroup,
  FormLabel,
} from "@wasp/auth/forms/internal/Form";

export const SignupPage = () => {
  return (
    <div className="container">
      <main>
        <h1>Signup</h1>
        <SignupForm
          additionalFields={({ register, formState: { errors } }) => {
            return (
              <FormItemGroup>
                <FormLabel>Address</FormLabel>
                <FormInput
                  {...register("address", {
                    required: "Address is required",
                  })}
                  type="text"
                />
                {errors.address && (
                  <FormError>{errors.address.message}</FormError>
                )}
              </FormItemGroup>
            );
          }}
        />
      </main>
    </div>
  );
};
