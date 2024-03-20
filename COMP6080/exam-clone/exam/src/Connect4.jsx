import React from "react"
import './App.css';

const Connect = (props) => {
  const containerCount = 100;
  const squares = Array.from({ length: containerCount }, (_, index) => (
    <div key={index} className="square"></div>
  ));

  return (
    <div className="connect-page">
      <div className="connect-container">{squares}</div>
    </div>
  );
}

export default Connect;