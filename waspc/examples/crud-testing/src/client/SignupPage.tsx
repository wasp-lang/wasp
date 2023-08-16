import { SignupForm } from "@wasp/auth/forms/Signup";
import {
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
        // additionalFields={({ register }) => {
        //   return (
        //     <FormItemGroup>
        //       <FormLabel>Address</FormLabel>
        //       <FormInput
        //         {...register("address")}
        //         type="text"
        //         placeholder="Address"
        //       />
        //     </FormItemGroup>
        //   );
        // }}
        />
      </main>
    </div>
  );
};
