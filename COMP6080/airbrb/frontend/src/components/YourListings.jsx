import React from 'react';
import Button from '@mui/material/Button';
import { Box } from '@mui/material';
import { Outlet, useNavigate, } from 'react-router-dom';
import config from '../config.json';

import SendIcon from '@mui/icons-material/Send';
import HomeIcon from '@mui/icons-material/Home';
import DeleteIcon from '@mui/icons-material/Delete';
import EditIcon from '@mui/icons-material/Edit';
import './styles/app.css'
import ButtonGroup from '@mui/material/ButtonGroup';

const YourListings = (props) => {
  const [listings, setListings] = React.useState([]);
  const [published, setPublished] = React.useState([]);
  const navigate = useNavigate();

  const getListings = async () => {
    const response = await fetch(`http://localhost:${config.BACKEND_PORT}/listings`, {
      method: 'GET',
      headers: {
        'Content-type': 'application/json',
        Authorization: `Bearer ${props.token}`,
      },
    });
    const data = await response.json();
    if (response.ok) {
      const yourListings = data.listings.filter(x => x.owner === props.email);
      setListings(yourListings);
      for (const i of yourListings) {
        try {
          const publishing = await getInfoListing(i.id)
          if (publishing) {
            setPublished(prevPublish => [...prevPublish, i.id]);
          }
        } catch (error) {
          console.log('Error getting info listing');
        }
      }
    } else {
      alert(data.error);
    }
  };

  const getInfoListing = async (listingId) => {
    try {
      const response = await fetch(`http://localhost:${config.BACKEND_PORT}/listings/${listingId}`, {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json',
          Authorization: `Bearer ${props.token}`,
        },
      });
      if (!response.ok) {
        throw new Error('Failed to fetch data');
      }
      const data = await response.json();
      return data.listing.published;
    } catch (error) {
      console.error('Error fetching listing information:', error.message);
      throw error;
    }
  }

  const deleteListing = async (listingId) => {
    const confirm = window.confirm('Are you sure you would like to delete this listing?');

    if (confirm) {
      const response = await fetch(`http://localhost:${config.BACKEND_PORT}/listings/${listingId}`, {
        method: 'DELETE',
        headers: {
          'Content-Type': 'application/json',
          Authorization: `Bearer ${props.token}`
        },
      });
      const data = await response.json()
      if (response.ok) {
        getListings();
      } else {
        throw new Error(data.error);
      }
    }
  }

  const unpublishListing = async (listingId) => {
    const response = await fetch(`http://localhost:${config.BACKEND_PORT}/listings/unpublish/${listingId}`, {
      method: 'PUT',
      headers: {
        'Content-type': 'application/json',
        Authorization: `Bearer ${props.token}`
      },
    });

    try {
      const data = await response.json();
      if (response.ok) {
        alert('Listing has been unpublished');
        navigate('/dashboard/alllistings')
      } else if (data.error) {
        alert(data.error);
      }
    } catch (err) {
      alert('Error in unpublishing listing');
    }
  }

  React.useEffect(() => {
    getListings();
  }, []);

  return (
    <>
      <Box className='listings-page'>
        {listings?.map((item) => (
          <div key={item.id} className='indiv-listing'>
            <img src={item.thumbnail} alt={item.title} className='indiv-listing-img highlight-on-hover-img' onClick={() => navigate(`/dashboard/alllistings/${item.id}`)}/>
            <Box className='listing-details' style={{ height: '210px', marginBottom: '5px' }}>
              <p style={{ fontSize: 'small' }}><i>{published.includes(item.id) ? 'Published' : 'Not published'}</i></p>
              <h3 style={{ textAlign: 'center', color: 'white' }}>{item.title}</h3>
              <div style={{ display: 'flex', alignItems: 'center', justifyContent: 'flex-start', gap: '8px' }}>
                <HomeIcon /><p style={{ margin: '0px' }}>{item.address}</p>
              </div>
              <p><i><b>${item.price} AUD per night</b></i></p>
            </Box>
            <ButtonGroup variant="contained" aria-label="outlined primary button group">
              <Button color="success" endIcon={<EditIcon />} onClick={() => navigate(`${item.id}`)}>Edit</Button>
              <Button color="success" endIcon={<DeleteIcon />} onClick={() => deleteListing(item.id)}>Delete</Button>
              {
                published.includes(item.id)
                  ? (
                  <Button color="success" endIcon={<SendIcon />} onClick={() => unpublishListing(item.id)}>Unpublish</Button>
                    )
                  : (
                  <Button color="success" endIcon={<SendIcon />} onClick={() => navigate(`/dashboard/publishlisting/${item.id}`)}>Publish</Button>
                    )
              }
            </ButtonGroup>
          </div>
        ))}
      </Box>
      <Outlet />
    </>
  )
}

export default YourListings;
