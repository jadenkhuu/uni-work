import React from 'react';
import { Box, Button, TextField } from '@mui/material';
import Typography from '@mui/material/Typography';
import config from '../config.json';
import {
  Outlet,
  useParams,
  useNavigate
} from 'react-router-dom';
import './styles/createedit.css'

const EditListing = (props) => {
  const { id } = useParams();
  const [title, setTitle] = React.useState('');

  const [address, setAddress] = React.useState('');
  const [suburb, setSuburb] = React.useState('');
  const [postcode, setPostcode] = React.useState('');
  const [state, setState] = React.useState('');

  const [price, setPrice] = React.useState('');
  const [thumbnail, setThumbnail] = React.useState(null);
  const [property, setProperty] = React.useState('');
  const [bathroom, setBathroom] = React.useState('');
  const [bedroom, setBedroom] = React.useState('');
  const [beds, setBeds] = React.useState('');

  const [thumbnailPreview, setThumbnailPreview] = React.useState('')
  const [images, setImages] = React.useState([]);
  const [imagePreviews, setImagePreviews] = React.useState([]);

  const [amenities, setAmenities] = React.useState([{ amenity: '' }]);
  const [amenityList, setList] = React.useState([]);

  const addAmenity = () => {
    setAmenities([...amenities, { amenity: '' }]);
  };

  const removeAmenity = (index) => {
    const amenity = [...amenities];
    amenity.splice(index, 1);
    setAmenities(amenity);
  };

  const handleAmenities = (e, index) => {
    const name = 'amenity';
    const amenity = [...amenities];
    amenity[index][name] = e.target.value;
    setAmenities(amenity);
    createAmenityArray(amenity);
  };

  const createAmenityArray = (amenities) => {
    const list = [];
    for (const i of amenities) {
      list.push(i.amenity);
    }
    setList(list);
  }

  const navigate = useNavigate();

  const handleThumbnail = (event) => {
    const file = event.target.files[0];
    if (file) {
      const reader = new FileReader();
      reader.onloadend = () => {
        setThumbnail(reader.result);
        setThumbnailPreview(reader.result);
      };
      reader.readAsDataURL(file);
    }
  };

  const handleImage = (event) => {
    const files = Array.from(event.target.files).slice(0, 5);
    Promise.all(files.map(file => {
      return new Promise((resolve, reject) => {
        const reader = new FileReader();
        reader.onload = (e) => resolve(e.target.result);
        reader.onerror = (e) => reject(e);
        reader.readAsDataURL(file);
      });
    }))
      .then(i => {
        setImagePreviews(i);
        setImages(i);
      })
      .catch(error => console.error(error));
  };

  const removeImage = (index) => {
    const newImages = [...images];
    const newImagePreviews = [...imagePreviews];
    newImages.splice(index, 1);
    newImagePreviews.splice(index, 1);
    setImages(newImages);
    setImagePreviews(newImagePreviews);
  };

  // Edit listing
  const editListing = async () => {
    const response = await fetch(`http://localhost:${config.BACKEND_PORT}/listings/${id}`, {
      method: 'PUT',
      headers: {
        'Content-type': 'application/json',
        Authorization: `Bearer ${props.token}`
      },
      body: JSON.stringify({
        title,
        address: [address, suburb, postcode, state].join(', '),
        thumbnail,
        price,
        metadata: { property, bathroom, bedroom, beds, amenityList, images }
      })

    });

    try {
      const data = await response.json();
      if (response.ok) {
        alert('Listing has been edited')
        navigate('/dashboard/yourlistings');
      } else if (data.error) {
        alert(data.error);
      }
    } catch (err) {
      alert('Error in editing listing');
    }
  }

  React.useEffect(() => {
    return () => {
      if (thumbnailPreview) {
        URL.revokeObjectURL(thumbnailPreview);
      }
    };
  }, [thumbnailPreview]);

  return (
    <>
      <Box className='create-listing-page'>
        {/* turn into Box */}
        <TextField label='Title' type="text" value={title} placeholder="Property Name" onChange={(e) => setTitle(e.target.value)} required/>
        <br />
        <Box style={{ display: 'flex', flexGrow: '1' }}>
          <TextField style={{ width: '25%' }} label='Address' type="text" value={address} placeholder="House number, street name" onChange={(e) => setAddress(e.target.value)} required/>
          <TextField style={{ width: '25%' }} label='Suburb' type="text" value={suburb} placeholder="Suburb" onChange={(e) => setSuburb(e.target.value)} required/>
          <TextField style={{ width: '25%' }} label='Postcode' type="text" value={postcode} placeholder="Postcode" onChange={(e) => setPostcode(e.target.value)} required/>
          <TextField style={{ width: '25%' }} label='State' type="text" value={state} placeholder="State" onChange={(e) => setState(e.target.value)} required/>
        </Box>
        <br />
        <TextField label='Price' type="number" min="0" value={price} placeholder="Property Price Per Night" onChange={(e) => setPrice(e.target.value)} required/>
        <br />
        <TextField label='Property Type' type="text" value={property} placeholder="Property Type" onChange={(e) => setProperty(e.target.value)} required/>
        <br />
        <TextField label='No. of Bathrooms' type="number" min="0" value={bathroom} placeholder="Please enter a number" onChange={(e) => setBathroom(e.target.value)} required/>
        <br />
        <TextField label='No. of Bedrooms' type="number" min="0" value={bedroom} placeholder="Please enter a number" onChange={(e) => setBedroom(e.target.value)} required/>
        <br />
        <TextField label='No. of Beds' type="number" min="0" value={beds} placeholder="Please enter a number" onChange={(e) => setBeds(e.target.value)} required/>
        <br />
        <Button color="success" variant="contained" component="label">
          Upload A Thumbnail
          <input type="file" onChange={handleThumbnail} required hidden/>
        </Button>
        {thumbnail && (
          <Box>
            <Typography variant="subtitle1">{thumbnail.name}</Typography>
            <img src={thumbnailPreview} alt="Thumbnail preview" style={{ maxWidth: '100px', maxHeight: '100px' }} />
          </Box>
        )}
        <Button color="success" variant="contained" component="label">
          Upload up to 5 additional images
          <input type="file" onChange={handleImage} multiple required hidden/>
        </Button>
        <Box display="flex" flexDirection="row">
          {imagePreviews.map((src, index) => (
            <Box key={index} position="relative" m={1}>
              <img src={src} alt={`Preview ${index}`} style={{ maxWidth: '100px', maxHeight: '100px' }} />
              <Button
                color="success"
                onClick={() => removeImage(index)}
                style={{
                  position: 'absolute',
                  top: 0,
                  right: 0,
                  minWidth: '32px',
                  height: '32px',
                  color: 'red',
                  fontWeight: 'bold',
                }}
              >X</Button>
            </Box>
          ))}
        </Box>
        {amenities.map((singleAmenity, index) => (
          <div key={index} >
            <div>
              <TextField label='Amenity' type="text" value={singleAmenity.input} placeholder="Property Amenities" onChange={(e) => handleAmenities(e, index)} required/>
              {amenities.length - 1 === index && (
                <Button color="success" variant='outlined' type="button" onClick={addAmenity}>+</Button>
              )}
            </div>
            <div>
              {amenities.length !== 1 && (
                <Button color="success" variant='outlined' type="button" onClick={() => removeAmenity(index)}>Remove Amenity</Button>
              )}
            </div>
          </div>
        ))}
        <br />
        <Button color="success" variant='outlined' onClick={editListing}>Submit Edited Listing</Button>
        <Button color="success" variant='outlined' onClick={() => navigate('/dashboard/yourlistings')}>Back</Button>
      </Box>
      <Outlet />
    </>
  )
};

export default EditListing;
