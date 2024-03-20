 './App.css';
import React from 'react';
import {
  BrowserRouter,
  Routes,
  Route,
  Navigate,
} from 'react-router-dom';
import Navbar from './Navbar';
import Dashboard from './Dashboard'
import RandomOperators from './RandomOperators'
import Connect from './Connect4'
import Memorisation from './Memorisation'

function App() {
  const [screenWidth, setScreenWidth] = React.useState(window.innerWidth)
  const [gamesWon, setGamesWon] = React.useState(() => {
    const savedGamesWon = localStorage.getItem('gamesWon');
    return savedGamesWon !== null ? parseInt(savedGamesWon, 10) : 0;
  });
  const [gamesRemaining, setGamesRemaining] = React.useState(() => {
    const savedNumber = localStorage.getItem('gamesRemaining');
    return savedNumber !== null ? parseInt(savedNumber, 10) : null;
  });

  React.useEffect(() => {
    const resizeScreen = () => {
      setScreenWidth(window.innerWidth);
    };
    window.addEventListener('resize', resizeScreen);
    return () => {
      window.removeEventListener('resize', resizeScreen);
    };


  }, []);

  React.useEffect(() => {
    localStorage.setItem('gamesWon', gamesWon.toString());
  }, [gamesWon]);

  React.useEffect(() => {
    localStorage.setItem('gamesRemaining', gamesRemaining.toString());
  }, [gamesRemaining]);

  return (
    <BrowserRouter>
      <Navbar screenWidth={screenWidth} setScreenWidth={setScreenWidth} />
      <Routes>
        <Route path="/" element={<Navigate to="/dashboard" />} />
        <Route path="/dashboard" element={<Dashboard gamesWon={gamesWon} setGamesWon={setGamesWon} gamesRemaining={gamesRemaining} setGamesRemaining={setGamesRemaining} />} />
        <Route path="/game/math" element={<RandomOperators gamesWon={gamesWon} setGamesWon={setGamesWon} gamesRemaining={gamesRemaining} setGamesRemaining={setGamesRemaining} />} />
        <Route path="/game/connect" element={<Connect gamesWon={gamesWon} setGamesWon={setGamesWon} gamesRemaining={gamesRemaining} setGamesRemaining={setGamesRemaining} />} />
        <Route path="/game/memory" element={<Memorisation gamesWon={gamesWon} setGamesWon={setGamesWon} gamesRemaining={gamesRemaining} setGamesRemaining={setGamesRemaining} />} />
      </Routes>
  </BrowserRouter>
  );
}

export default App;
