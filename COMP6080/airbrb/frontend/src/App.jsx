import React from 'react';
import Dashboard from './components/Dashboard'
import Login from './components/Login';
import Register from './components/Register';
import YourListings from './components/YourListings';
import AllListings from './components/AllListings';
import CreateListing from './components/CreateListing';
import EditListing from './components/EditListing';
import PublishListing from './components/PublishListing'
import ViewListing from './components/ViewListing';
import SearchListing from './components/SearchListings';

import {
  BrowserRouter,
  Routes,
  Route,
  Navigate,
} from 'react-router-dom';
import './components/styles/app.css';

const App = () => {
  const [token, setToken] = React.useState(null);
  const [email, setEmail] = React.useState('');

  const [search, setSearch] = React.useState('');
  const [searchLocation, setSearchLocation] = React.useState('');
  const [searchBeds, setSearchBeds] = React.useState('');
  const [searchMinPrice, setMinPrice] = React.useState('');
  const [searchMaxPrice, setMaxPrice] = React.useState('');

  // React.useEffect(() => {
  //   if (localStorage.getItem('token')) {
  //     setToken(localStorage.getItem('token'));
  //     localStorage.removeItem('token')
  //   }
  // }, [])

  React.useEffect(() => {
    document.body.style.margin = '0';
    document.body.style.padding = '0';
    return () => {
      document.body.style.margin = '';
      document.body.style.padding = '';
    };
  }, []);

  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<Navigate to="dashboard/alllistings" />} />
        <Route path="/dashboard" element={<Dashboard
          email={email} setEmail={setEmail}
          token={token} setToken={setToken}
          search={search} setSearch={setSearch}
          searchLocation={searchLocation} setSearchLocation={setSearchLocation}
          searchBeds={searchBeds} setSearchBeds={setSearchBeds}
          searchMinPrice={searchMinPrice} setMinPrice={setMinPrice}
          searchMaxPrice={searchMaxPrice} setMaxPrice={setMaxPrice}/>}>
          <Route path="alllistings" element={
            <AllListings
              email={email} setEmail={setEmail}
              token={token} setToken={setToken}
              search={search} setSearch={setSearch}
              searchLocation={searchLocation} setSearchLocation={setSearchLocation}
              searchBeds={searchBeds} setSearchBeds={setSearchBeds}
              searchMinPrice={searchMinPrice} setMinPrice={setMinPrice}
              searchMaxPrice={searchMaxPrice} setMaxPrice={setMaxPrice}/>}/>
          <Route path="search" element={
            <SearchListing
              email={email} setEmail={setEmail}
              token={token} setToken={setToken}
              search={search} setSearch={setSearch}
              searchLocation={searchLocation} setSearchLocation={setSearchLocation}
              searchBeds={searchBeds} setSearchBeds={setSearchBeds}
              searchMinPrice={searchMinPrice} setMinPrice={setMinPrice}
              searchMaxPrice={searchMaxPrice} setMaxPrice={setMaxPrice}/>}/>
          <Route path="alllistings/:id" element={<ViewListing email={email} setEmail={setEmail} token={token} setToken={setToken}/>}/>
          <Route path="yourlistings" element={<YourListings email={email} setEmail={setEmail} token={token} setToken={setToken}/>}/>
          <Route path="yourlistings/:id" element={<EditListing email={email} setEmail={setEmail} token={token} setToken={setToken}/>}/>
          <Route path="publishlisting/:id" element={<PublishListing email={email} setEmail={setEmail} token={token} setToken={setToken}/>}/>
          <Route path="create" element={<CreateListing token={token} setToken={setToken}/>}/>
        </Route>
        <Route path="/login" element={<Login token={token} setToken={setToken} email={email} setEmail={setEmail}/>} />
        <Route path="/register" element={<Register token={token} setToken={setToken} email={email} setEmail={setEmail}/>} />
      </Routes>
    </BrowserRouter>

  )
}

export default App;
