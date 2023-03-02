const inputFieldClasses = `
  box-border w-full border-2 border-neutral-300
  bg-neutral-100/50
  hover:bg-neutral-200
  focus:bg-neutral-100/50 focus:outline-none
  focus:border-yellow-500
  transition ease-out duration-200
  h-10 px-2 text-sm placeholder:text-neutral-500
`
const EmailAndPassForm = ({ title, userField, passField, setUser, setPass, handleSignup }) => (
    <div className='w-full text-center'>
        <h2 className='text-base font-bold text-neutral-600'>
          {title}
        </h2>
      <form onSubmit={handleSignup} className='block flex flex-col'>
        <input
          className={inputFieldClasses + ' mt-5'}
          type='text'
          placeholder='Enter email address'
          value={userField}
          onChange={e => setUser(e.target.value)}
        />
        <input
          className={inputFieldClasses + ' mt-4'}
          type='password'
          placeholder='Enter password'
          value={passField}
          onChange={e => setPass(e.target.value)}
        />
        <input
          className={`
            bg-yellow-500 h-10 mt-3
            text-white text-sm font-bold
            hover:bg-yellow-400
          `}
          type='submit'
          value='Sign up'
        />
      </form>
    </div>
  )

  export default EmailAndPassForm