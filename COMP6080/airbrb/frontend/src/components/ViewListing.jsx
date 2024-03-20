// import Button from '@mui/material/Button';
import React from 'react';
import config from '../config.json';
import { useParams, Outlet, useNavigate } from 'react-router-dom';
import BathtubIcon from '@mui/icons-material/Bathtub';
import HotelIcon from '@mui/icons-material/Hotel';
import ChevronRightIcon from '@mui/icons-material/ChevronRight';
import './styles/viewlistings.css'
import { Button, } from '@mui/material';

const ViewListing = (props) => {
  const listingId = useParams().id;
  const [listingObj, setListing] = React.useState({});
  const [meta, setMeta] = React.useState({})
  const navigate = useNavigate()

  const listing = async () => {
    const response = await fetch(`http://localhost:${config.BACKEND_PORT}/listings/${listingId}`, {
      method: 'GET',
      headers: {
        'Content-type': 'application.json',
        Authorization: `Bearer ${props.token}`,
      },
    });
    const data = await response.json();
    if (response.ok) {
      setListing(data.listing);
      setMeta(data.listing.metadata);
    } else {
      throw new Error(data.error);
    }
  }

  React.useEffect(() => {
    listing();
  }, [listingId]);

  return (
    <>
    <div className='view-listing-page'>
      <div className='view-listing-left' style={{ marginRight: '5%', }}>
        <img src={listingObj.thumbnail} alt={listingObj.title} className='main-img'/>
        <div style={{ display: 'flex' }}>
          {meta.images?.map((image, index) => (
            <img key={index} src={image} alt={`additional image ${index + 1}`} className="add-imgs" />
          ))}
        </div>
      </div>
      <div style={{ display: 'flex', width: '100%' }}>
        <div className='view-listing-mid' style={{ marginRight: '5%' }}>
          <h2>{listingObj.title}</h2>
          <p><i>{listingObj.address}</i></p>
          <p><b>Property Type: {meta.property}</b></p>
          <div style={{ backgroundColor: '#3a7754', color: 'white', display: 'flex', flexDirection: 'column', padding: '10px', paddingRight: '10px', borderRadius: '5px', justifyContent: 'center', alignItems: 'center' }}>
            <b>{meta.bedroom} Bedrooms</b>
            <div style={{ fontWeight: 'bold', display: 'flex', alignItems: 'center', justifyContent: 'flex-start', gap: '8px' }}>
              <HotelIcon />{meta.beds}
              <BathtubIcon />{meta.bathroom}
            </div>
          </div>
          <p style={{ fontSize: 'large' }}><b>${listingObj.price} per night</b></p>
          Owner: {listingObj.owner}<br />
          <br />
          <Button variant='outlined' color="success" onClick={() => navigate('/dashboard/alllistings')}>Back</Button>
        </div>
        <div className='view-listing-right' style={{ backgroundColor: '#3a7754', borderRadius: '20px', color: 'white', padding: '0 20px', marginLeft: 'auto', marginRight: '5%' }}>
          <h2>Amenities</h2>
          {meta.amenityList?.map((item) => (
            <div key={item} style={{ fontWeight: 'bold', display: 'flex', alignItems: 'center', justifyContent: 'flex-start', gap: '8px' }}>
              <ChevronRightIcon />{item}
            </div>
          ))}
        </div>
      </div>
    </div>
      <Outlet />
    </>
  )
}

export default ViewListing;
