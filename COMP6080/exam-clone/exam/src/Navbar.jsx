import React from "react"
import './App.css';

const Navbar = (props) => {
  return (
    <div className="page">
      <div className="nav-bar">
        <div className="nav-logo"></div>
        { props.screenWidth > 1400
        ?
        <>
          <a href="/dashboard" className="nav-item">Dashboard</a>
          <a href="/game/math" className="nav-item">Math</a>
          <a href="/game/connect" className="nav-item">Connect 4</a>
          <a href="/game/memory" className="nav-item">Memorisation</a>
        </>
        :
        <>
          <a href="/dashboard" className="nav-item">Da</a>
          <a href="/game/math" className="nav-item">Ma</a>
          <a href="/game/connect" className="nav-item">Co</a>
          <a href="/game/memory" className="nav-item">Me</a>
        </>
        }
      </div>
    </div>
  )
}

export default Navbar;