import React from 'react';
import {
  useNavigate,
} from 'react-router-dom';
import TextField from '@mui/material/TextField';
import Button from '@mui/material/Button';
import './styles/loginregister.css';
import config from '../config.json';

// Allows the user to sign in with their valid email and passwords
const Login = (props) => {
  const [password, setPassword] = React.useState('');

  const token = localStorage.getItem('token');
  const navigate = useNavigate();

  // Grabs the login details and compares it to the data stored
  const login = async () => {
    const response = await fetch(`http://localhost:${config.BACKEND_PORT}/user/auth/login`, {
      method: 'POST',
      headers: {
        'Content-type': 'application/json',
        Authorization: `Bearer ${token}`
      },
      body: JSON.stringify({
        email: props.email,
        password,
      })
    });
    const data = await response.json();
    if (data.token) {
      props.setToken(data.token);
      navigate('/dashboard/alllistings');
    } else if (data.error) {
      alert(data.error);
      console.log(data.error);
      navigate('/login');
    }
  }

  const handleKeyPress = (e) => {
    if (e.key === 'Enter') {
      login();
    }
  };

  return (
    <div className='page'>
      <div className='login-block'>
        <h2>airbrb - Log In</h2>
        <TextField
          label="Email"
          value={props.email}
          onChange={(e) => props.setEmail(e.target.value)}
        />
        <br />
        <TextField
          label="Password"
          type='password'
          value={password}
          onChange={(e) => setPassword(e.target.value)}
          onKeyPress={(e) => handleKeyPress(e)}
        />
        <br />
        <Button variant="outlined" onClick={login}>Log In</Button>
        <br /><br />
        <span>Need an account? <a href="/register">Register</a></span>
        <br />
        <span><a href="/dashboard/alllistings">Go back</a></span>
      </div>
    </div>
  );
}

export default Login;
