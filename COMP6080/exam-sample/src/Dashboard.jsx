import React from "react"
import './App.css';
import { Link } from "react-router-dom";

const Dashboard = (props) => {

  return (
    <div className="page">
      <div className="banner">
        <div className="logo"></div>
        {/* <img className="logo" src="../public/logo192.PNG" alt="logo"/> */}
        <div className="banner-buttons">
          { props.screenWidth > 800
          ?
          <div className="link-container">
            <div><Link to='/'>Home</Link></div> | <div><Link to='/Blanko'>Blanko</Link></div> | <div><Link to='/Slido'>Slido</Link></div> | <div><Link to='/Tetro'>Tetro</Link></div>
          </div>
          :
          <div className="link-container">
            <div><Link to='/'>H</Link></div> | <div><Link to='/Blanko'>B</Link></div> | <div><Link to='/Slido'>S</Link></div> | <div><Link to='/Tetro'>T</Link></div>
          </div>
          }
        </div>
      </div>
    </div>
  )
}

export default Dashboard