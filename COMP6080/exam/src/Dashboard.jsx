import React from "react"
import './App.css';

const Dashboard = (props) => {

  const fetchGamesRemaining = async () => {
    const response = await fetch('https://cgi.cse.unsw.edu.au/~cs6080/raw/data/remain.json');
    const data = await response.json();
    if (response.ok) {
      props.setGamesRemaining(data.score);
    } else {
      props.setGamesRemaining(5);
    }
  }

  React.useEffect(() => {
    if (props.gamesRemaining === null) {
      fetchGamesRemaining();
    }
  }, []);

  const resetButton = () => {
    props.setGamesWon(0);
    fetchGamesRemaining();
  }

  return (
    <div className="dashboard">
      <div class="grid-container">
        <div class="box games-remaining">Games remaining: {props.gamesRemaining}</div>
        <div class="box">Games won: {props.gamesWon}</div>
        <div class="box">
          { props.gamesRemaining > 0
          ?
          <>
            Keep Going
          </>
          :
          <>
            Great Job
          </>
          }
        </div>
        <div class="box">
          <button
            style={{ width: '30%', height: '15%', backgroundColor: '#fff', fontWeight: 'bold', cursor: 'pointer' }}
            onClick={resetButton}
          >
            Reset
          </button>
        </div>
      </div>
    </div>
  )
}

export default Dashboard;