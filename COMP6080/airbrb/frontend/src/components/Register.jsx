import React from 'react';
import {
  useNavigate
} from 'react-router-dom';
import TextField from '@mui/material/TextField';
import Button from '@mui/material/Button';
import './styles/loginregister.css';
import config from '../config.json';

const Register = (props) => {
  const [password, setPassword] = React.useState('')
  const [confirmPassword, setConfirmPassword] = React.useState('')
  const [name, setName] = React.useState('')

  const navigate = useNavigate();

  // If token exists, automatically navigate to dashboard page
  // React.useEffect(() => {
  //   if (localStorage.getItem('token')) {
  //     navigate('/dashboard/alllistings');
  //   }
  // }, [localStorage.getItem('token')]);

  const register = async () => {
    if (confirmPassword !== password) {
      alert('Passwords do not match')
      navigate('/register');
      return
    }
    const regex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/
    if (!regex.test(props.email)) {
      alert('Invalid email')
      navigate('/register');
      return
    }

    const response = await fetch(`http://localhost:${config.BACKEND_PORT}/user/auth/register`, {
      method: 'POST',
      headers: {
        'Content-type': 'application/json',
      },
      body: JSON.stringify({
        email: props.email,
        password,
        name,
      })
    });

    const data = await response.json();
    if (data.token) {
      localStorage.setItem('token', data.token);
      props.setToken(data.token);
      navigate('/dashboard/alllistings');
    } else if (data.error) {
      alert(data.error);
      console.log(data.error);
      navigate('/register');
    }
  }

  const handleKeyPress = (e) => {
    if (e.key === 'Enter') {
      register();
    }
  };

  return (
    <>
    <div className='page'>
      <div className='register-block'>
        <h2>airbrb - Register</h2>
        <TextField label="email" value={props.email} onChange={(e) => props.setEmail(e.target.value)} />
        <br />
        <TextField label="name" value={name} onChange={(e) => setName(e.target.value)} />
        <br />
        <TextField label="password" type='password' value={password} onChange={(e) => setPassword(e.target.value)} />
        <br />
        <TextField
          label="confirm password"
          type='password'
          value={confirmPassword}
          onChange={(e) => setConfirmPassword(e.target.value)}
          onKeyPress={(e) => handleKeyPress(e)}
        />
        <br />
        <Button variant="outlined" onClick={register}>Register</Button>
        <br /><br />
        <span>Have an account? <a href="/login">Log in</a></span>
        <br />
        <span><a href="/dashboard/alllistings">Go back</a></span>
      </div>
    </div>
    </>
  );
}

export default Register;
