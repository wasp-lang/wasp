import { useState } from 'react'
import {
  FormError,
  FormInput,
  FormItemGroup,
  FormLabel,
  SubmitButton,
} from '@wasp/auth/forms/internal/Form'
import { useForm } from 'react-hook-form'
import customSubmit from '@wasp/actions/customSignup'

export const SignupPage = () => {
  const {
    register,
    handleSubmit,
    formState: { errors },
  } = useForm<{
    username: string
    password: string
    address: string
  }>()
  const [message, setMessage] = useState<{
    type: 'success' | 'error'
    text: string
  } | null>(null)

  const onSubmit = handleSubmit(async (data) => {
    try {
      const result = await customSubmit(data)
      console.error('result', result)
      if (result.success) {
        setMessage({
          type: 'success',
          text: 'Signup successful. You can now login.',
        })
      } else {
        setMessage({
          type: 'error',
          text: result.message,
        })
      }
    } catch (error: any) {
      const { message, data } = error.data
      setMessage({
        type: 'error',
        text: `${message}: ${data.message}`,
      })
    }
  })

  return (
    <>
      <h1>Custom Signup Action</h1>
      <form onSubmit={onSubmit}>
        {message && (
          <div
            style={{
              color: message.type === 'error' ? 'red' : 'green',
              marginBottom: '1rem',
            }}
          >
            {message.text}
          </div>
        )}
        <FormItemGroup>
          <FormLabel>Username</FormLabel>
          <FormInput {...register('username')} />
          <FormError>{errors.username?.message}</FormError>
        </FormItemGroup>
        <FormItemGroup>
          <FormLabel>Password</FormLabel>
          <FormInput {...register('password')} />
          <FormError>{errors.password?.message}</FormError>
        </FormItemGroup>
        <FormItemGroup>
          <FormLabel>Address</FormLabel>
          <FormInput {...register('address')} />
          <FormError>{errors.address?.message}</FormError>
        </FormItemGroup>
        <SubmitButton type="submit">Signup</SubmitButton>
      </form>
    </>
  )
}
