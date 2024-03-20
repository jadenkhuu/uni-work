import './App.css';
import React from 'react';
import {
  BrowserRouter,
  Routes,
  Route,
  Navigate,
} from 'react-router-dom';
import Dashboard from './Dashboard';

function App() {
  const [screenWidth, setScreenWidth] = React.useState(window.innerWidth)

  React.useEffect(() => {
    const handleResize = () => {
      setScreenWidth(window.innerWidth);
    };

    window.addEventListener('resize', handleResize);

    return () => {
      window.removeEventListener('resize', handleResize);
    };
  }, []);

  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<Navigate to="dashboard" />} />
        <Route path="/dashboard" element={<Dashboard screenWidth={screenWidth} setScreenWidth={setScreenWidth} />} />
      </Routes>
    </BrowserRouter>
  );
}

export default App;
