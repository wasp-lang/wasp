import { useAuthContext } from "@wasp.sh/lib-auth/browser";
import { useForm, type UseFormReturn } from "react-hook-form";
import { useNavigate } from "react-router";

import { isMethodEnabled, signInUrl } from "../../../actions.js";
import { getClientOptions } from "../../../runtime.js";
import type { OAuthProviderName } from "../../../types.js";
import type {
  AdditionalSignupField,
  AdditionalSignupFieldRenderFn,
  AdditionalSignupFields,
  FormState,
  LoginSignupFormFields,
} from "../../types.js";
import {
  Form,
  FormError,
  FormInput,
  FormItemGroup,
  FormLabel,
  FormTextarea,
  SubmitButton,
} from "../Form.js";
import "../auth-styles.css";
import { useEmail } from "../email/useEmail.js";
import { SocialButton } from "../social/SocialButton.js";
import * as SocialIcons from "../social/SocialIcons.js";
import { useUsernameAndPassword } from "../usernameAndPassword/useUsernameAndPassword.js";
import { clsx } from "../util.js";
import styles from "./LoginSignupForm.module.css";

const socialIcons: Record<OAuthProviderName, () => React.JSX.Element> = {
  slack: SocialIcons.Slack,
  discord: SocialIcons.Discord,
  google: SocialIcons.Google,
  keycloak: SocialIcons.Keycloak,
  github: SocialIcons.GitHub,
  microsoft: SocialIcons.Microsoft,
};
const socialOrder: OAuthProviderName[] = [
  "slack",
  "discord",
  "google",
  "keycloak",
  "github",
  "microsoft",
];

export const LoginSignupForm = ({
  state,
  socialButtonsDirection = "horizontal",
  additionalSignupFields,
}: {
  state: "login" | "signup";
  socialButtonsDirection?: "horizontal" | "vertical";
  additionalSignupFields?: AdditionalSignupFields;
}) => {
  const { isLoading, setErrorMessage, setSuccessMessage, setIsLoading } =
    useAuthContext();
  const options = getClientOptions();
  const isLogin = state === "login";
  const cta = isLogin ? "Log in" : "Sign up";
  const navigate = useNavigate();
  const isEmailEnabled = isMethodEnabled("email");
  const isUsernameEnabled = isMethodEnabled("usernameAndPassword");
  const enabledSocial = socialOrder.filter((name) => isMethodEnabled(name));
  const isSocialAuthEnabled = enabledSocial.length > 0;
  const isAnyPasswordBasedAuthEnabled = isEmailEnabled || isUsernameEnabled;

  const onErrorHandler = (
    error: Error & { data?: { data?: { message?: string } } },
  ) => {
    setErrorMessage({
      title: error.message,
      description: error.data?.data?.message,
    });
  };
  const hookForm = useForm<LoginSignupFormFields>();
  const {
    register,
    formState: { errors },
    handleSubmit: hookFormHandleSubmit,
  } = hookForm;
  const usernameAndPassword = useUsernameAndPassword({
    isLogin,
    onError: onErrorHandler,
    onSuccess() {
      navigate(options.onAuthSucceededRedirectTo);
    },
  });
  const email = useEmail({
    isLogin,
    onError: onErrorHandler,
    showEmailVerificationPending() {
      hookForm.reset();
      setSuccessMessage(
        `You've signed up successfully! Check your email for the confirmation link.`,
      );
    },
    onLoginSuccess() {
      navigate(options.onAuthSucceededRedirectTo);
    },
  });
  const handleSubmit = isEmailEnabled
    ? email.handleSubmit
    : usernameAndPassword.handleSubmit;

  async function onSubmit(data: LoginSignupFormFields) {
    setIsLoading(true);
    setErrorMessage(null);
    setSuccessMessage(null);
    try {
      await handleSubmit(data as never);
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
            {enabledSocial.map((name) => {
              const Icon = socialIcons[name];
              return (
                <SocialButton key={name} href={signInUrl(name)}>
                  <Icon />
                </SocialButton>
              );
            })}
          </div>
        </div>
      )}
      {isSocialAuthEnabled && isAnyPasswordBasedAuthEnabled && (
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
          {isUsernameEnabled && (
            <FormItemGroup>
              <FormLabel>Username</FormLabel>
              <FormInput
                {...register("username", { required: "Username is required" })}
                type="text"
                disabled={isLoading}
              />
              {errors.username && (
                <FormError>{errors.username.message}</FormError>
              )}
            </FormItemGroup>
          )}
          {isEmailEnabled && (
            <FormItemGroup>
              <FormLabel>E-mail</FormLabel>
              <FormInput
                {...register("email", { required: "Email is required" })}
                type="email"
                disabled={isLoading}
              />
              {errors.email && <FormError>{errors.email.message}</FormError>}
            </FormItemGroup>
          )}
          <FormItemGroup>
            <FormLabel>Password</FormLabel>
            <FormInput
              {...register("password", { required: "Password is required" })}
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

  function renderField(
    field: AdditionalSignupField,
    Component: any,
    props?: Record<string, unknown>,
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

  if (typeof additionalSignupFields === "function") {
    return (additionalSignupFields as AdditionalSignupFieldRenderFn)(hookForm, {
      isLoading,
    });
  }

  return (
    additionalSignupFields &&
    additionalSignupFields.map((field) => {
      if (typeof field === "function") {
        return field(hookForm, { isLoading });
      }
      switch (field.type) {
        case "input":
          return renderField(field, FormInput, { type: "text" });
        case "textarea":
          return renderField(field, FormTextarea);
        default:
          throw new Error(
            `Unsupported additional signup field type: ${(field as AdditionalSignupField).type}`,
          );
      }
    })
  );
}
