import { Outlet, useNavigate, } from 'react-router-dom';
import React from 'react';
import './styles/app.css';
import config from '../config.json';
import { Box, } from '@mui/material';

const SearchListing = (props) => {
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
      let list = data.listings;
      if (props.search) {
        list = list.filter(listing => listing.title.includes(props.search));
      }
      if (props.search !== '') {
        list = list.filter(listing => listing.title.includes(props.search))
      }
      if (props.searchLocation !== '') {
        list = list.filter(listing => listing.address.includes(props.searchLocation))
      }
      if (props.searchMinPrice !== '') {
        list = list.filter(listing => parseInt(listing.price) >= parseInt(props.searchMinPrice))
      }
      if (props.searchMaxPrice !== '') {
        list = list.filter(listing => parseInt(listing.price) <= parseInt(props.searchMaxPrice))
      }

      let filterFetchList = await Promise.all(
        list.map(async (listing) => {
          const response = await fetch(`http://localhost:${config.BACKEND_PORT}/listings/${listing.id}`, {
            method: 'GET',
            headers: {
              'Content-type': 'application/json',
              Authorization: `Bearer ${props.token}`,
            },
          });
          const data = await response.json();
          if (response.ok) {
            return { ...listing, beds: data.listing.metadata.beds, };
          } else {
            alert(data.error);
            return listing;
          }
        })
      );
      if (props.searchBeds) {
        filterFetchList = filterFetchList.filter(l => parseInt(l.beds) === parseInt(props.searchBeds));
      }
      setListings(filterFetchList);
    } else {
      throw new Error(data.error);
    }
  };

  React.useEffect(() => {
    getListings();
  }, [props.search, props.searchLocation, props.searchMinPrice, props.searchMaxPrice, props.searchBeds, props.token]);

  return (
    <>
      <Box className='listings-page'>
        {listings?.map((item) => (
          <div
            key={item.id}
            id={item.id}
            className="indiv-listing highlight-on-hover"
            style={{ height: '500px' }}
            onClick={() => navigate(`/dashboard/alllistings/${item.id}`)}
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

export default SearchListing;
