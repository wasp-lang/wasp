import { useForm, type UseFormReturn } from "react-hook-form";
import { useNavigate } from "react-router";
import { useAuthContext } from "../../../context.js";
import { getAuthErrorMessage } from "../../authErrorMessage.js";
import type { AuthFormsConfig, SocialProviderName } from "../../config.js";
import { emailFieldRules, emailInputProps } from "../../emailField.js";
import {
  Form,
  FormError,
  FormInput,
  FormItemGroup,
  FormLabel,
  FormTextarea,
  SubmitButton,
} from "../../Form.js";
import { SocialButton } from "../../social/SocialButton.js";
import {
  Discord,
  GitHub,
  Google,
  Keycloak,
  Microsoft,
  Slack,
} from "../../social/SocialIcons.js";
import type {
  AdditionalSignupField,
  AdditionalSignupFieldRenderFn,
  AdditionalSignupFields,
  FormState,
  LoginSignupFormFields,
} from "../../types.js";
import { clsx } from "../../util.js";
import { useEmail } from "../email/useEmail.js";
import { useUsernameAndPassword } from "../usernameAndPassword/useUsernameAndPassword.js";
import styles from "./LoginSignupForm.module.css";

const socialIcons: Record<SocialProviderName, () => React.JSX.Element> = {
  slack: Slack,
  discord: Discord,
  google: Google,
  keycloak: Keycloak,
  github: GitHub,
  microsoft: Microsoft,
};

export const LoginSignupForm = ({
  config,
  state,
  socialButtonsDirection = "horizontal",
  additionalSignupFields,
}: {
  config: AuthFormsConfig;
  state: "login" | "signup";
  socialButtonsDirection?: "horizontal" | "vertical";
  additionalSignupFields?: AdditionalSignupFields;
}) => {
  const { isLoading, setErrorMessage, setSuccessMessage, setIsLoading } =
    useAuthContext();
  const isLogin = state === "login";
  const cta = isLogin ? "Log in" : "Sign up";
  const navigate = useNavigate();
  const onErrorHandler = (error: unknown) => {
    setErrorMessage(getAuthErrorMessage(error));
  };
  const hookForm = useForm<LoginSignupFormFields>();
  const {
    register,
    formState: { errors },
    handleSubmit: hookFormHandleSubmit,
  } = hookForm;

  const isSocialAuthEnabled = config.socialProviders.length > 0;
  const isUsernameAndPasswordAuthEnabled =
    config.usernameAndPassword !== undefined;
  const isEmailAuthEnabled = config.email !== undefined;
  const isAnyPasswordBasedAuthEnabled =
    isUsernameAndPasswordAuthEnabled || isEmailAuthEnabled;
  const areBothSocialAndPasswordBasedAuthEnabled =
    isSocialAuthEnabled && isAnyPasswordBasedAuthEnabled;

  const { handleSubmit } = getPasswordBasedSubmitHandler();

  // We assert the data's type below because the form holds raw input strings,
  // while the signup data type uses the return types of the user's signup field
  // getters (e.g. `Promise<string>` for an async getter).
  // TODO: Close the type gap between the forms and the user's signup fields,
  // so this assertion can go away.
  function getPasswordBasedSubmitHandler(): {
    handleSubmit: ((data: LoginSignupFormFields) => Promise<void>) | null;
  } {
    if (config.usernameAndPassword) {
      const { handleSubmit } = useUsernameAndPassword({
        actions: config.usernameAndPassword,
        isLogin,
        onError: onErrorHandler,
        onSuccess() {
          navigate(config.onAuthSucceededRedirectTo);
        },
      });
      return {
        handleSubmit: (data) =>
          handleSubmit(data as Parameters<typeof handleSubmit>[0]),
      };
    }
    if (config.email) {
      const { handleSubmit } = useEmail({
        actions: config.email,
        isLogin,
        onError: onErrorHandler,
        showEmailVerificationPending() {
          hookForm.reset();
          setSuccessMessage(
            `You've signed up successfully! Check your email for the confirmation link.`,
          );
        },
        onLoginSuccess() {
          navigate(config.onAuthSucceededRedirectTo);
        },
      });
      return {
        handleSubmit: (data) =>
          handleSubmit(data as Parameters<typeof handleSubmit>[0]),
      };
    }
    return { handleSubmit: null };
  }

  async function onSubmit(data: LoginSignupFormFields): Promise<void> {
    if (!handleSubmit) {
      return;
    }
    setIsLoading(true);
    setErrorMessage(null);
    setSuccessMessage(null);
    try {
      await handleSubmit(data);
    } finally {
      setIsLoading(false);
    }
  }

  return (
    <>
      {isSocialAuthEnabled && (
        <div className={styles.socialAuth}>
          <div className={styles.socialAuthLabel}>{cta} with</div>
          <div
            className={clsx(
              styles.socialAuthButtons,
              styles[socialButtonsDirection],
            )}
          >
            {config.socialProviders.map((provider) => {
              const Icon = socialIcons[provider.name];
              return (
                <SocialButton key={provider.name} href={provider.signInUrl}>
                  <Icon />
                </SocialButton>
              );
            })}
          </div>
        </div>
      )}
      {areBothSocialAndPasswordBasedAuthEnabled && (
        <div className={styles.orContinueWith}>
          <div className={styles.orContinueWithLineContainer}>
            <div className={styles.orContinueWithLine} />
          </div>
          <div className={styles.orContinueWithTextContainer}>
            <span className={styles.orContinueWithText}>Or continue with</span>
          </div>
        </div>
      )}
      {isAnyPasswordBasedAuthEnabled && (
        <Form onSubmit={hookFormHandleSubmit(onSubmit)}>
          {isUsernameAndPasswordAuthEnabled && (
            <FormItemGroup>
              <FormLabel>Username</FormLabel>
              <FormInput
                {...register("username", {
                  required: "Username is required",
                })}
                type="text"
                disabled={isLoading}
              />
              {errors.username && (
                <FormError>{errors.username.message}</FormError>
              )}
            </FormItemGroup>
          )}
          {isEmailAuthEnabled && (
            <FormItemGroup>
              <FormLabel>E-mail</FormLabel>
              <FormInput
                {...register("email", emailFieldRules)}
                {...emailInputProps}
                disabled={isLoading}
              />
              {errors.email && <FormError>{errors.email.message}</FormError>}
            </FormItemGroup>
          )}
          <FormItemGroup>
            <FormLabel>Password</FormLabel>
            <FormInput
              {...register("password", {
                required: "Password is required",
              })}
              type="password"
              disabled={isLoading}
            />
            {errors.password && (
              <FormError>{errors.password.message}</FormError>
            )}
          </FormItemGroup>
          <AdditionalFormFields
            hookForm={hookForm}
            formState={{ isLoading }}
            additionalSignupFields={additionalSignupFields}
          />
          <FormItemGroup>
            <SubmitButton type="submit" disabled={isLoading}>
              {cta}
            </SubmitButton>
          </FormItemGroup>
        </Form>
      )}
    </>
  );
};

function AdditionalFormFields({
  hookForm,
  formState: { isLoading },
  additionalSignupFields,
}: {
  hookForm: UseFormReturn<LoginSignupFormFields>;
  formState: FormState;
  additionalSignupFields?: AdditionalSignupFields;
}) {
  const {
    register,
    formState: { errors },
  } = hookForm;

  function renderField<ComponentType extends React.JSXElementConstructor<any>>(
    field: AdditionalSignupField,
    // Ideally we would use ComponentType here, but it doesn't work with react-hook-form
    Component: any,
    props?: React.ComponentProps<ComponentType>,
  ) {
    const errorMessage = errors[field.name]?.message;
    return (
      <FormItemGroup key={field.name}>
        <FormLabel>{field.label}</FormLabel>
        <Component
          {...register(field.name, field.validations)}
          {...props}
          disabled={isLoading}
        />
        {errorMessage && <FormError>{errorMessage}</FormError>}
      </FormItemGroup>
    );
  }

  if (areAdditionalFieldsRenderFn(additionalSignupFields)) {
    return additionalSignupFields(hookForm, { isLoading });
  }

  return (
    additionalSignupFields &&
    additionalSignupFields.map((field) => {
      if (isFieldRenderFn(field)) {
        return field(hookForm, { isLoading });
      }
      switch (field.type) {
        case "input":
          return renderField<typeof FormInput>(field, FormInput, {
            type: "text",
          });
        case "textarea":
          return renderField<typeof FormTextarea>(field, FormTextarea);
        default:
          throw new Error(
            `Unsupported additional signup field type: ${field.type}`,
          );
      }
    })
  );
}

function isFieldRenderFn(
  additionalSignupField: AdditionalSignupField | AdditionalSignupFieldRenderFn,
): additionalSignupField is AdditionalSignupFieldRenderFn {
  return typeof additionalSignupField === "function";
}

function areAdditionalFieldsRenderFn(
  additionalSignupFields?: AdditionalSignupFields,
): additionalSignupFields is AdditionalSignupFieldRenderFn {
  return typeof additionalSignupFields === "function";
}
