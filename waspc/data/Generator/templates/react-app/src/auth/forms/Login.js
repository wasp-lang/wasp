import { useState } from 'react'

import login from '../login.js'

const LoginForm = ({ onSuccess = () => undefined, onError = () => undefined } = {}) => {
  const [usernameFieldVal, setUsernameFieldVal] = useState('');
  const [passwordFieldVal, setPasswordFieldVal] = useState('');

  const handleLogin = async (event) => {
    event.preventDefault();
    try {
      await login(usernameFieldVal, passwordFieldVal);
      onSuccess();
    } catch (err) {
      onError();
    }
  };

  return (
    <form onSubmit={handleLogin} className="login-form auth-form">
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
        <input type="submit" value="Log in"/>
      </div>
    </form>
  )
}

export default LoginForm;
