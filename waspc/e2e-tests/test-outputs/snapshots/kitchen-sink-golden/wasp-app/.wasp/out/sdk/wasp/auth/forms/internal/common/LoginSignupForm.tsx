import { useForm, type UseFormReturn } from 'react-hook-form'
import styles from './LoginSignupForm.module.css'
import { config } from '../../../../client/index.js'
import {
  clsx,
  Form,
  FormInput,
  FormPasswordInput,
  FormItemGroup,
  FormLabel,
  FormError,
  FormTextarea,
  SubmitButton,
  type AdditionalSignupFields,
  type AdditionalSignupField,
  type AdditionalSignupFieldRenderFn,
  type FormState,
} from '@wasp.sh/lib-sdk-core/browser'

import { useAuthContext } from '@wasp.sh/lib-auth/browser'
import { emailFieldRules, emailInputProps } from '@wasp.sh/lib-sdk-core/browser'
import * as SocialIcons from '@wasp.sh/lib-sdk-core/browser'
import { SocialButton } from '@wasp.sh/lib-sdk-core/browser'
import { useNavigate } from 'react-router'
import { getAuthErrorMessage } from '@wasp.sh/lib-sdk-core/browser'
import { useEmail } from '../email/useEmail'

const slackSignInUrl = `${config.apiUrl}/auth/slack/login`
const discordSignInUrl = `${config.apiUrl}/auth/discord/login`
const googleSignInUrl = `${config.apiUrl}/auth/google/login`
const gitHubSignInUrl = `${config.apiUrl}/auth/github/login`
const microsoftSignInUrl = `${config.apiUrl}/auth/microsoft/login`

import type { LoginSignupFormFields } from '@wasp.sh/lib-sdk-core/browser'


// PRIVATE API
export const LoginSignupForm = ({
    state,
    socialButtonsDirection = 'horizontal',
    additionalSignupFields,
}: {
    state: 'login' | 'signup'
    socialButtonsDirection?: 'horizontal' | 'vertical'
    additionalSignupFields?: AdditionalSignupFields
}) => {
  const {
    isLoading,
    setErrorMessage,
    setSuccessMessage,
    setIsLoading,
  } = useAuthContext();
  const isLogin = state === 'login'
  const cta = isLogin ? 'Log in' : 'Sign up';
  const navigate = useNavigate();
  const onErrorHandler = (error: unknown) => {
    setErrorMessage(getAuthErrorMessage(error));
  };
  const hookForm = useForm<LoginSignupFormFields>()
  const { register, formState: { errors }, handleSubmit: hookFormHandleSubmit } = hookForm
  const { handleSubmit } = useEmail({
    isLogin,
    onError: onErrorHandler,
    showEmailVerificationPending() {
      hookForm.reset()
      setSuccessMessage(`You've signed up successfully! Check your email for the confirmation link.`)
    },
    onLoginSuccess() {
      navigate("/")
    },
  });
  async function onSubmit(data: LoginSignupFormFields): Promise<void> {
    setIsLoading(true);
    setErrorMessage(null);
    setSuccessMessage(null);
    try {
      // We assert the type because the form holds raw input strings, while the
      // signup data type uses the return types of the user's signup field
      // getters (e.g. `Promise<string>` for an async getter).
      // TODO: Close the type gap between the forms and the user's signup fields,
      // so this assertion can go away.
      await handleSubmit(data as Parameters<typeof handleSubmit>[0]);
    } finally {
      setIsLoading(false);
    }
  }

  return (<>
        <div className={styles.socialAuth}>
          <div className={styles.socialAuthLabel}>{cta} with</div>
            <div className={clsx(styles.socialAuthButtons, styles[socialButtonsDirection])}>
              <SocialButton href={slackSignInUrl}><SocialIcons.Slack/></SocialButton>

              <SocialButton href={discordSignInUrl}><SocialIcons.Discord/></SocialButton>

              <SocialButton href={googleSignInUrl}><SocialIcons.Google/></SocialButton>


              <SocialButton href={gitHubSignInUrl}><SocialIcons.GitHub/></SocialButton>

              <SocialButton href={microsoftSignInUrl}><SocialIcons.Microsoft/></SocialButton>
          </div>
        </div>
        <div className={styles.orContinueWith}>
          <div className={styles.orContinueWithLineContainer}>
            <div className={styles.orContinueWithLine}/>
          </div>
          <div className={styles.orContinueWithTextContainer}>
            <span className={styles.orContinueWithText}>Or continue with</span>
          </div>
        </div>
        <Form onSubmit={hookFormHandleSubmit(onSubmit)}>
          <FormItemGroup>
            <FormLabel>E-mail</FormLabel>
            <FormInput
              {...register('email', emailFieldRules)}
              {...emailInputProps}
              disabled={isLoading}
            />
            {errors.email && <FormError>{errors.email.message}</FormError>}
          </FormItemGroup>
          <FormItemGroup>
            <FormLabel>Password</FormLabel>
            <FormPasswordInput
              {...register('password', {
                required: 'Password is required',
              })}
              disabled={isLoading}
            />
            {errors.password && <FormError>{errors.password.message}</FormError>}
          </FormItemGroup>
          <AdditionalFormFields
            hookForm={hookForm}
            formState={{ isLoading }}
            additionalSignupFields={additionalSignupFields}
          />
          <FormItemGroup>
            <SubmitButton type="submit" disabled={isLoading}>{cta}</SubmitButton>
          </FormItemGroup>
        </Form>
  </>)
}

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
    props?: React.ComponentProps<ComponentType>
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
        {errorMessage && (
          <FormError>{errorMessage}</FormError>
        )}
      </FormItemGroup>
    );
  }

  if (areAdditionalFieldsRenderFn(additionalSignupFields)) {
    return additionalSignupFields(hookForm, { isLoading })
  }

  return (
    additionalSignupFields &&
    additionalSignupFields.map((field) => {
      if (isFieldRenderFn(field)) {
        return field(hookForm, { isLoading })
      }
      switch (field.type) {
        case 'input':
          return renderField<typeof FormInput>(field, FormInput, {
            type: 'text',
          })
        case 'textarea':
          return renderField<typeof FormTextarea>(field, FormTextarea)
        default:
          throw new Error(
            `Unsupported additional signup field type: ${field.type}`
          )
      }
    })
  )
}

function isFieldRenderFn(
  additionalSignupField: AdditionalSignupField | AdditionalSignupFieldRenderFn
): additionalSignupField is AdditionalSignupFieldRenderFn {
  return typeof additionalSignupField === 'function'
}

function areAdditionalFieldsRenderFn(
  additionalSignupFields?: AdditionalSignupFields
): additionalSignupFields is AdditionalSignupFieldRenderFn {
  return typeof additionalSignupFields === 'function'
}
