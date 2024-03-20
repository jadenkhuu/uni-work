import React from 'react';
import { Box } from '@mui/material';
import { Outlet, useNavigate } from 'react-router-dom';
import config from '../config.json';
import './styles/app.css'

const AllListings = (props) => {
  console.log(props)
  const [listings, setListings] = React.useState([]);
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
      const filteredListings = [];
      for (const i of data.listings) {
        const isPublished = await getInfoListing(i.id);
        if (isPublished) {
          filteredListings.push(i);
        }
      }
      setListings(filteredListings);
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

  React.useEffect(() => {
    getListings();
  }, [location.state]);

  return (
    <>
      <Box className='listings-page'>
        {listings?.map((item) => (
          <div
            key={item.id}
            id={item.id}
            className="indiv-listing highlight-on-hover"
            style={{ height: '500px', cursor: 'pointer' }}
            onClick={() => navigate(`${item.id}`)}
            >
            <img className='indiv-listing-img' src={item.thumbnail} alt={item.title} style={{ width: '320px' }} />
            <Box className='listing-details' sx={{ height: '150px' }}>
              <h3 style={{ textAlign: 'center', color: 'white' }}>{item.title}</h3>
              <p><i><b>${item.price} AUD per night</b></i></p>
              <p><i><b>Reviews</b></i></p>
            </Box>
          </div>
        ))}
      </Box>
      <Outlet />
    </>
  )
}

export default AllListings;
