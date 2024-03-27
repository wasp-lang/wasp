import { useContext } from 'react';
import { useForm } from 'react-hook-form';
import { styled } from 'wasp/core/stitches.config';
import { config } from 'wasp/client';
import { AuthContext } from '../../Auth';
import { FormInput, FormItemGroup, FormLabel, FormError, FormTextarea, } from '../Form';
import * as SocialIcons from '../social/SocialIcons';
import { SocialButton } from '../social/SocialButton';
const SocialAuth = styled('div', {
    marginTop: '1.5rem'
});
const SocialAuthLabel = styled('div', {
    fontWeight: '500',
    fontSize: '$sm'
});
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
});
const googleSignInUrl = `${config.apiUrl}/auth/google/login`;
// PRIVATE API
export const LoginSignupForm = ({ state, socialButtonsDirection = 'horizontal', additionalSignupFields, }) => {
    const { isLoading, setErrorMessage, setSuccessMessage, setIsLoading, } = useContext(AuthContext);
    const isLogin = state === 'login';
    const cta = isLogin ? 'Log in' : 'Sign up';
    const hookForm = useForm();
    const { register, formState: { errors }, handleSubmit: hookFormHandleSubmit } = hookForm;
    return (<>
        <SocialAuth>
          <SocialAuthLabel>{cta} with</SocialAuthLabel>
          <SocialAuthButtons gap='large' direction={socialButtonsDirection}>
              <SocialButton href={googleSignInUrl}><SocialIcons.Google /></SocialButton>


          </SocialAuthButtons>
        </SocialAuth>
  </>);
};
function AdditionalFormFields({ hookForm, formState: { isLoading }, additionalSignupFields, }) {
    const { register, formState: { errors }, } = hookForm;
    function renderField(field, 
    // Ideally we would use ComponentType here, but it doesn't work with react-hook-form
    Component, props) {
        return (<FormItemGroup key={field.name}>
        <FormLabel>{field.label}</FormLabel>
        <Component {...register(field.name, field.validations)} {...props} disabled={isLoading}/>
        {errors[field.name] && (<FormError>{errors[field.name].message}</FormError>)}
      </FormItemGroup>);
    }
    if (areAdditionalFieldsRenderFn(additionalSignupFields)) {
        return additionalSignupFields(hookForm, { isLoading });
    }
    return (additionalSignupFields &&
        additionalSignupFields.map((field) => {
            if (isFieldRenderFn(field)) {
                return field(hookForm, { isLoading });
            }
            switch (field.type) {
                case 'input':
                    return renderField(field, FormInput, {
                        type: 'text',
                    });
                case 'textarea':
                    return renderField(field, FormTextarea);
                default:
                    throw new Error(`Unsupported additional signup field type: ${field.type}`);
            }
        }));
}
function isFieldRenderFn(additionalSignupField) {
    return typeof additionalSignupField === 'function';
}
function areAdditionalFieldsRenderFn(additionalSignupFields) {
    return typeof additionalSignupFields === 'function';
}
//# sourceMappingURL=LoginSignupForm.jsx.map