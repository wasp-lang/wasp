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
} from '@wasp/lib-sdk-core/browser'

import { useAuthContext } from '@wasp/lib-auth/browser'
import { useNavigate } from 'react-router'
import { peekMcpReturnTo } from '../../../../client/mcp/returnTo.js'
import { getAuthErrorMessage, newPasswordFieldRules, passwordFieldRules } from '@wasp/lib-sdk-core/browser'
import { passwordValidationSchema } from '../../../validation.js'
import { usernameFieldRules } from '@wasp/lib-sdk-core/browser'
import { useUsernameAndPassword } from '../usernameAndPassword/useUsernameAndPassword'


import type { LoginSignupFormFields } from '@wasp/lib-sdk-core/browser'


// PRIVATE API
export const LoginSignupForm = ({
    state,
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
  const { handleSubmit } = useUsernameAndPassword({
    isLogin,
    onError: onErrorHandler,
    onSuccess() {
      navigate(peekMcpReturnTo() ?? "/")
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
        <Form onSubmit={hookFormHandleSubmit(onSubmit)}>
          <FormItemGroup>
            <FormLabel>Username</FormLabel>
            <FormInput
              {...register('username', usernameFieldRules)}
              type="text"
              disabled={isLoading}
            />
            {errors.username && <FormError>{errors.username.message}</FormError>}
          </FormItemGroup>
          <FormItemGroup>
            <FormLabel>Password</FormLabel>
            <FormPasswordInput
              {...register('password', isLogin ? passwordFieldRules : newPasswordFieldRules(passwordValidationSchema))}
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
