import React, { useState } from 'react'


function Block({ value, handleClick }) {
    return (
        <button className="block" onClick={handleClick}>
            {value}
        </button>
    )
}

function Game() {
    const [blocks, setBlocks] = useState(Array(9).fill(null));
    const [isXTurn, setXTurn] = useState(true);
    const nextTurn = isXTurn ? 'x' : 'o';
    const winner = calculateWinner(blocks);

    function renderBlock(i) {
        return (
            <Block value={blocks[i]}
                handleClick={
                    () => {
                        if (blocks[i] != null || winner != null) {
                            return;
                        }
                        const updatedBlocks = [...blocks];
                        updatedBlocks[i] = nextTurn;
                        setBlocks(updatedBlocks);
                        setXTurn(!isXTurn);
                    }
                } />
        )
    }

    function setStatus() {
        if (winner) {
            return `Winner is ${winner} 🎊`;
        }
        else if (isBoardFull(blocks)) {
            return `It's draw ❗️`;
        }
        else {
            return `Next Turn: ${nextTurn}`;
        }
    }

    function restartGame() {
        setBlocks(Array(9).fill(null));
        setXTurn(true);
    }

    return (
        <div className="board">
            <h2>{setStatus()}</h2>
            <div className="row">
                {renderBlock(0)}
                {renderBlock(1)}
                {renderBlock(2)}
            </div>
            <div className="row">
                {renderBlock(3)}
                {renderBlock(4)}
                {renderBlock(5)}
            </div>
            <div className="row">
                {renderBlock(6)}
                {renderBlock(7)}
                {renderBlock(8)}
            </div>
            <button className="restart" onClick={restartGame}>Restart Game</button>
        </div>
    )
}

function calculateWinner(blocks) {
    const possibleWins = [
        [0, 1, 2],
        [3, 4, 5],
        [6, 7, 8],
        [0, 3, 6],
        [1, 4, 7],
        [2, 5, 8],
        [0, 4, 8],
        [2, 4, 6],
    ];

    for (let i = 0; i < possibleWins.length; i++) {
        const [x, y, z] = possibleWins[i];
        if (blocks[x] && blocks[x] === blocks[y] && blocks[x] === blocks[z]) {
            return blocks[x];
        }
    }
    
    return null;
}

function isBoardFull(blocks) {
    for (let i = 0; i < blocks.length; i++) {
        if (blocks[i] == null) {
            return false;
        }
    }
    return true;
}

export default Game;
