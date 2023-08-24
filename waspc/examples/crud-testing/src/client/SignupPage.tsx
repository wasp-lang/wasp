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
          additionalFields={[
            {
              name: "address",
              label: "Address",
              type: "input",
              validations: {
                required: "Address is required",
              },
            },
          ]}
          // additionalFieldsRender={(
          //   { register, formState: { errors } },
          //   { isLoading }
          // ) => (
          //   <>
          //     <FormItemGroup>
          //       <FormLabel>Phone number</FormLabel>
          //       <FormInput
          //         {...register("phone", {
          //           required: "Phone number is required",
          //         })}
          //         type="text"
          //         disabled={isLoading}
          //       />
          //       {errors.phone && <FormError>{errors.phone.message}</FormError>}
          //     </FormItemGroup>
          //   </>
          // )}
        />
      </main>
    </div>
  );
};
