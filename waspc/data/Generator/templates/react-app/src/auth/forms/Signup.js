import { useState } from 'react';

import signup from '../signup.js';
import login from '../login.js';

const SignupForm = ({ onSuccess = () => undefined, onError = () => undefined } = {}) => {
  const [usernameFieldVal, setUsernameFieldVal] = useState('');
  const [passwordFieldVal, setPasswordFieldVal] = useState('');

  const handleSignup = async (event) => {
    event.preventDefault();
    try {
      await signup({ username: usernameFieldVal, password: passwordFieldVal });
      await login (usernameFieldVal, passwordFieldVal);

      setUsernameFieldVal('');
      setPasswordFieldVal('');
      onSuccess();
    } catch (err) {
      onError(err);
    }
  }

  return (
    <form onSubmit={handleSignup} className='signup-form auth-form'>
      <h2>Username</h2>
      <input
        type="text"
        value={usernameFieldVal}
        onChange={e => setUsernameFieldVal(e.target.value)}
      />
      <h2>Password</h2>
      <input
        type="password"
        value={passwordFieldVal}
        onChange={e => setPasswordFieldVal(e.target.value)}
      />
      <div>
        <input type="submit" value="Sign up"/>
      </div>
    </form>
  )
}

export default SignupForm;
