import { useContext } from 'react'
import { useForm, UseFormReturn } from 'react-hook-form'

import { AuthContext } from '../../Auth'
import { Form, FormInput, FormItemGroup, FormLabel, FormError, FormTextarea, SubmitButton } from '../Form'
import type { AdditionalSignupFields, AdditionalSignupFieldsRender, FormState } from '../../types'
import * as SocialIcons from '../social/SocialIcons'
import { SocialButton } from '../social/SocialButton'

const SocialAuth = styled('div', {
    marginTop: '1.5rem'
})

const SocialAuthLabel = styled('div', {
    fontWeight: '500',
    fontSize: '$sm'
})

const SocialAuthButtons = styled('div', {
    marginTop: '0.5rem',
    display: 'flex',

    variants: {
        direction: {
            horizontal: {
                display: 'grid',
                gridTemplateColumns: 'repeat(auto-fit, minmax(48px, 1fr))',
            },
            vertical: {
                flexDirection: 'column',
                margin: '8px 0',
            }
        },
        gap: {
            small: {
                gap: '4px',
            },
            medium: {
                gap: '8px',
            },
            large: {
                gap: '16px',
            }
        }
    }
})
const googleSignInUrl = `${config.apiUrl}/auth/google/login`

export type LoginSignupFormFields = {
  [key: string]: string;
}

export const LoginSignupForm = ({
    state,
    socialButtonsDirection = 'horizontal',
    additionalSignupFields,
    additionalSignupFieldsRender,
}: {
    state: 'login' | 'signup'
    socialButtonsDirection?: 'horizontal' | 'vertical'
    additionalSignupFields?: AdditionalSignupFields
    additionalSignupFieldsRender?: AdditionalSignupFieldsRender
}) => {
  const {
    isLoading,
    setErrorMessage,
    setSuccessMessage,
    setIsLoading,
  } = useContext(AuthContext)
  const isLogin = state === 'login'
  const cta = isLogin ? 'Log in' : 'Sign up';
  const hookForm = useForm<LoginSignupFormFields>()
  const { register, formState: { errors }, handleSubmit: hookFormHandleSubmit } = hookForm

  return (<>
        <SocialAuth>
          <SocialAuthLabel>{cta} with</SocialAuthLabel>
          <SocialAuthButtons gap='large' direction={socialButtonsDirection}>
              <SocialButton href={googleSignInUrl}><SocialIcons.Google/></SocialButton>

          </SocialAuthButtons>
        </SocialAuth>
  </>)
}

// If users define both additionalSignupFields and additionalSignupFieldsRender
// we render both
function AdditionalFormFields({
  hookForm,
  formState: { isLoading },
  additionalSignupFields,
  additionalSignupFieldsRender,
}: {
  hookForm: UseFormReturn<LoginSignupFormFields>;
  formState: FormState;
  additionalSignupFields: AdditionalSignupFields;
  additionalSignupFieldsRender: AdditionalSignupFieldsRender;
}) {
  const {
    register,
    formState: { errors },
  } = hookForm;

  function renderField<ComponentType extends React.JSXElementConstructor<any>>(
    field: AdditionalSignupFields[0],
    // Ideally we would use ComponentType here, but it doesn't work with react-hook-form
    Component: any,
    props?: React.ComponentProps<ComponentType>
  ) {
    return (
      <FormItemGroup key={field.name}>
        <FormLabel>{field.label}</FormLabel>
        <Component
          {...register(field.name, field.validations)}
          {...props}
          disabled={isLoading}
        />
        {errors[field.name] && (
          <FormError>{errors[field.name].message}</FormError>
        )}
      </FormItemGroup>
    );
  }

  return (
    <>
      {additionalSignupFields && additionalSignupFields.map((field) => {
        switch (field.type) {
          case "input":
            return renderField<typeof FormInput>(field, FormInput, {
              type: "text",
            });
          case "textarea":
            return renderField<typeof FormTextarea>(field, FormTextarea);
          default:
            throw new Error(
              `Unsupported additional signup field type: ${field.type}`
            );
        }
      })}
      {additionalSignupFieldsRender &&
        additionalSignupFieldsRender(hookForm, { isLoading })}
    </>
  );
}
