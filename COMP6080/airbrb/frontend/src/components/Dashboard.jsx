import * as React from 'react';
import Button from '@mui/material/Button';
import { Outlet, useNavigate, useLocation } from 'react-router-dom';
import AppBar from '@mui/material/AppBar';
import Toolbar from '@mui/material/Toolbar';
import Typography from '@mui/material/Typography';
import { Box, TextField } from '@mui/material';
import config from '../config.json';
import Modal from '@mui/material/Modal';
import './styles/app.css';

// import { DemoContainer } from '@mui/x-date-pickers/internals/demo';
// import { AdapterDayjs } from '@mui/x-date-pickers/AdapterDayjs';
// import { LocalizationProvider } from '@mui/x-date-pickers/LocalizationProvider';
// import { DatePicker } from '@mui/x-date-pickers/DatePicker';

const Dashboard = (props) => {
  const navigate = useNavigate();
  const location = useLocation();

  const Logout = async () => {
    await fetch(`http://localhost:${config.BACKEND_PORT}/user/auth/logout`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        Authorization: `Bearer ${props.token}`,
      },
    });
    props.setToken(null);
    navigate('/dashboard/alllistings');
  };

  const style = {
    fontFamily: 'Arial',
    display: 'flex',
    flexDirection: 'column',
    justifyContent: 'center',
    gap: '15px',
    position: 'absolute',
    top: '50%',
    left: '50%',
    transform: 'translate(-50%, -50%)',
    width: '340px',
    bgcolor: 'white',
    border: '5px solid #54ab79',
    borderRadius: '5px',
    boxShadow: 24,
    p: 4,
  };

  const [open, setOpen] = React.useState(false);
  const handleOpen = () => {
    props.setSearch('')
    props.setSearchLocation('')
    props.setSearchBeds('')
    props.setMinPrice('')
    props.setMaxPrice('')
    setOpen(true);
  }
  const handleClose = () => setOpen(false);

  return (
    <Box>
        <Box>
          <AppBar position="fixed" style={{ backgroundColor: '#54AB79' }} className='nav-bar' >
          <Toolbar>
            <div style={{ display: 'flex', flexGrow: 1 }}>
              <Typography variant="h5" component="div" onClick={() => navigate('/dashboard/alllistings')} style={{ cursor: 'pointer' }}>
                airbrb
              </Typography>
            </div>
            {
              location.pathname === '/dashboard/yourlistings'
                ? (
                <Box className='banner-buttons'>
                  <Button color="inherit" onClick={() => navigate('create')}>New Listing</Button>
                  <Button color="inherit" onClick={() => navigate('alllistings')}>All Listings</Button>
                  <Button color="inherit" onClick={Logout}>Log out</Button>
                </Box>
                  )
                : props.token
                  ? location.pathname === '/dashboard/search'
                    ? (
                      <Box className='banner-buttons'>
                        <Button color="inherit" onClick={() => navigate('alllistings')}>Back</Button>
                        <Button color="inherit" onClick={() => navigate('yourlistings')}>Your Listings</Button>
                        <Button color="inherit" onClick={Logout}>Log out</Button>
                      </Box>
                      )
                    : (
                      <Box className='banner-buttons'>
                        <Button color="inherit" onClick={() => handleOpen()}>Search</Button>
                        <Button color="inherit" onClick={() => navigate('yourlistings')}>Your Listings</Button>
                        <Button color="inherit" onClick={Logout}>Log out</Button>
                      </Box>
                      )
                  : location.pathname === '/dashboard/search'
                    ? (
                      <Box className='banner-buttons'>
                        <Button color="inherit" onClick={() => navigate('alllistings')}>Back</Button>
                        <Button color="inherit" onClick={() => navigate('/login')}>Log In</Button>
                        <Button color="inherit" onClick={() => navigate('/register')}>Register</Button>
                      </Box>
                      )
                    : (
                      <Box className='banner-buttons'>
                        <Button color="inherit" onClick={() => handleOpen()}>Search</Button>
                        <Button color="inherit" onClick={() => navigate('/login')}>Log In</Button>
                        <Button color="inherit" onClick={() => navigate('/register')}>Register</Button>
                      </Box>
                      )
            }
          </Toolbar>
          </AppBar>
          <Modal
            open={open}
            onClose={handleClose}
            aria-labelledby="modal-modal-title"
            aria-describedby="modal-modal-description"
          >
            <Box sx={style}>
              <TextField variant="outlined" label='Search Titles' type="text" style={{ backgroundColor: 'white' }}
                onChange={(e) => props.setSearch(e.target.value)} required/>
              <TextField variant="outlined" label='Search Locations' type="text" style={{ backgroundColor: 'white' }}
                onChange={(e) => props.setSearchLocation(e.target.value)} required/>
              <TextField label='No. of Beds' type="number" min="0" placeholder="Please enter a number"
                onChange={(e) => props.setSearchBeds(e.target.value)} required/>
              <div style={{ display: 'flex' }}>
                <TextField label='Min Price' type="number" min="0" placeholder="Please enter a min price"
                  onChange={(e) => props.setMinPrice(e.target.value)} required/>
                <TextField label='Max Price' type="number" min="0" placeholder="Please enter a max price" required
                  onChange={(e) => props.setMaxPrice(e.target.value)}/>
              </div>
              <Button color="success" variant="outlined"
                  onClick={() => {
                    navigate('alllistings')
                    navigate('search');
                    handleClose();
                  }}>Search</Button>
            </Box>
          </Modal>
        </Box>
        <Outlet />
    </Box>
  )
}

export default Dashboard;
