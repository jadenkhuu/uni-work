import React from "react"
import './App.css';

const RandomOperators = (props) => {
  const [randomNumber1, setRandomNumber1] = React.useState(null);
  const [operator, setOperator] = React.useState('')
  const [randomNumber3, setRandomNumber3] = React.useState(null);
  const [answer, setAnswer] = React.useState('');

  const operatorsArr = ['+', '-', '/', '*', "%"]
  const generateOperator = () => {
    const i = Math.floor(Math.random() * operatorsArr.length);
    return operatorsArr[i];
  };

  const generateNumber = () => Math.floor(Math.random() * 50) + 1;

  const resetGame = () => {
    setRandomNumber1(generateNumber());
    setRandomNumber3(generateNumber());
    setOperator(generateOperator());
    setAnswer('');
  };

  const calculateAns = () => {
    switch (operator) {
      case '+': return randomNumber1 + randomNumber3;
      case '-': return randomNumber1 - randomNumber3;
      case '/': return randomNumber1 / randomNumber3;
      case '*': return randomNumber1 * randomNumber3;
      case '%': return randomNumber1 % randomNumber3;
      default: return null;
    }
  };
  const checkAnswer = () => {
    const result = calculateAns();
    // Convert answer to a number and compare it to the result
    if (Number(answer) === result) {
      alert("Congratulations!");
      props.setGamesWon(props.gamesWon + 1);
      if (props.gamesRemaining > 0) {
        props.setGamesRemaining(props.gamesRemaining - 1);
      }
      resetGame();
    }
  };

  React.useEffect(() => {
    resetGame();
  }, []);

  return (
    <div className="math-page">
      <div className="math-container">
        <div class="math-child">{randomNumber1}</div>
        <div class="math-child">{operator}</div>
        <div class="math-child">{randomNumber3}</div>
        <div class="math-child">=</div>
        <div class="math-child">
          <input
            style={{ width: '25%' }}
            type="number"
            value={answer}
            onChange={(e) => setAnswer(e.target.value)}
            onKeyUp={checkAnswer}
          />
        </div>
      </div>
      <button onClick={resetGame} style={{ width: '10%', height: '5%', backgroundColor: '#fff', fontWeight: 'bold', cursor: 'pointer' }}>Reset</button>
    </div>
  )
}

export default RandomOperators;