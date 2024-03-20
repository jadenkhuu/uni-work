import React from 'react';
import {
  useParams,
  useNavigate
} from 'react-router-dom';
import config from '../config.json';
import { Button } from '@mui/material';
import { DemoContainer } from '@mui/x-date-pickers/internals/demo';
import dayjs from 'dayjs';
import { AdapterDayjs } from '@mui/x-date-pickers/AdapterDayjs';
import { LocalizationProvider } from '@mui/x-date-pickers/LocalizationProvider';
import { DatePicker } from '@mui/x-date-pickers/DatePicker';
import './styles/loginregister.css'

const PublishListing = (props) => {
  const { id } = useParams();
  const navigate = useNavigate()

  const getDate = (dateString) => {
    const dateObject = new Date(dateString);
    const day = dateObject.getDate();
    const month = dateObject.getMonth() + 1;
    const year = dateObject.getFullYear();
    const newDate = `${day}/${month}/${year}`;
    return newDate;
  }

  const today = dayjs().add(1, 'day');
  const tomorrow = dayjs().add(2, 'day');

  const [availabilities, setAvailability] = React.useState([{ start: getDate(today), end: '' }]);

  const getTime = (dateString) => {
    const [day, month, year] = dateString.split('/');
    const dateObject = new Date(`${month}/${day}/${year}`);
    const dayOfWeek = dateObject.toLocaleDateString('en-US', { weekday: 'short' });
    const monthName = dateObject.toLocaleDateString('en-US', { month: 'short' });
    const timeZoneOffset = dateObject.toString().match(/GMT[+-]\d{4}/)[0];
    const original = `${dayOfWeek} ${monthName} ${day} ${year} 00:00:00 ${timeZoneOffset} (Australian Eastern Daylight Time)`;
    return original;
  }

  const addAvailability = () => {
    setAvailability([...availabilities, { start: '', end: '' }]);
  };

  const removeAvailability = (index) => {
    const availability = [...availabilities];
    availability.splice(index, 1);
    setAvailability(availability);
  };

  const handleStartAvailability = (e, index) => {
    const availability = [...availabilities];
    availability[index].start = getDate(e.$d);
    setAvailability(availability);
  };

  const handleEndAvailability = (e, index) => {
    const availability = [...availabilities];
    availability[index].end = getDate(e.$d);
    setAvailability(availability);
  };

  // Edit listing
  const publishListing = async () => {
    // Check that at least one availability is present
    if (availabilities.length === 1 && availabilities[0].end === '') {
      alert('Please enter at least one available time for the listing.');
      return;
    } else if (availabilities.length > 1 && availabilities[0].end === '') {
      alert('Please enter your availability from the first input.');
      return;
    } else if (dayjs(getTime(availabilities[0].start)).$d > dayjs(getTime(availabilities[0].end)).$d) {
      alert('Please enter your start availability before your end availability.');
      return;
    }

    for (let i = 0; i < availabilities.length; i++) {
      if (i === 0) {
        continue;
      }

      if (availabilities[i].start === '' || availabilities[i].end === '') {
        alert(`Please fill in all available times for availability ${i + 1}.`);
        return;
      } else if (getTime(availabilities[i].start) === getTime(availabilities[i].end)) {
        alert(`The start date and end date of availability ${i} cannot be the same.`);
        return;
      } else if (dayjs(getTime(availabilities[i].start)).$d > (dayjs(getTime(availabilities[i].end))).$d) {
        alert(`The start date of availability ${i} cannot be after its end date.`);
        return;
      }
      if (dayjs(getTime(availabilities[i - 1].end)).$d > (dayjs(getTime(availabilities[i].start))).$d) {
        alert(`The start date of availability ${i + 1} must come after availability ${i}'s end date.`);
        return;
      }
    }

    const response = await fetch(`http://localhost:${config.BACKEND_PORT}/listings/publish/${id}`, {
      method: 'PUT',
      headers: {
        'Content-type': 'application/json',
        Authorization: `Bearer ${props.token}`
      },
      body: JSON.stringify({
        availability: availabilities
      })

    });

    try {
      const data = await response.json();
      if (response.ok) {
        alert('Listing has been published');
        navigate('/dashboard/yourlistings');
      } else if (data.error) {
        alert(data.error);
      }
    } catch (err) {
      alert('Error in publishing listing');
    }
  }
  return (
      <div className='publish-listing page' style={{ backgroundColour: 'white' }}>
        <div className='register-block'>
          <div>Select Your AirBrB Availabilities</div>
          <br />
          {availabilities.map((singleAvailability, index) => (
            <div key={index} style={{ display: 'flex', flexDirection: 'column', alignItems: 'center' }}>
              <div>
                <LocalizationProvider dateAdapter={AdapterDayjs}>
                  <DemoContainer label="Responsive variant" value={singleAvailability.input} components={['DatePicker']}>
                    <DatePicker defaultValue={today} disablePast label="Pick Start Date" onChange={(e) => handleStartAvailability(e, index)} required/>
                  </DemoContainer>
                  <DemoContainer label="Responsive variant" value={singleAvailability.input} components={['DatePicker']}>
                    <DatePicker defaultValue={tomorrow} disablePast label="Pick End Date" onChange={(e) => handleEndAvailability(e, index)} required/>
                  </DemoContainer>
                </LocalizationProvider>
                <div>
                  {availabilities.length !== 1 && (
                    <Button color="success" variant='outlined' type="button" onClick={() => removeAvailability(index)}>Remove Availability</Button>
                  )}
                </div>
                {availabilities.length - 1 === index && (
                  <Button color="success" variant='outlined' type="button" onClick={addAvailability}>Add availability</Button>
                )}
              </div>
            </div>
          ))}
          <Button color="success" variant='outlined' onClick={publishListing}>Publish Listing</Button>
          <Button color="success" variant='outlined' onClick={() => navigate('/dashboard/yourlistings')}>Back</Button>
        </div>
      </div>
  )
};

export default PublishListing;
