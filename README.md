# Tic-Tac-Toe

3x3 Tic-Tac-Toe console game (human vs computer, turn by turn) written in Scala.

## Info
The human player may choose:
- his/her own mark (__x__ or __o__)
- which player makes the first move (__human__ or __computer__)
- the difficulty of the computer (__easy__ or __hard__)
- to start a new game after a game has ended

## Computer Algorithms
The __easy__ computer just randomly selects a free position on the board and thus is easy to beat (in most cases).

The __hard__ computer is based on the NegaMax algorithm (which is a slight modification of the MiniMax algorithm) and thus is unbeatable.
[Here](https://www.youtube.com/watch?v=STjW3eH0Cik) is a very illustrative explanation of the MiniMax algorithm by Patrick Winston.

The NegaMax variation is simply based on the idea that 

```
max(a, b) = -min(-a, -b)
```

and thus there is no need to explicitly distinguish in which level the evaluation step of a move takes places. This is done by a value called `level` which equals `max=1` for the computer's turn or `min=-1` for the player's turn.

## Project Setup

1. Clone this repository to any destination path you want (for example '~/Downloads/tic-tac-toe').

2. Open terminal and navigate to this directory:
   ```
   $ cd ~/Downloads/tic-tac-toe
   ```
4. Compile project:
   ```
   $ sbt compile
   ```
5. Run tests:
   ```
   $ sbt test
   ```
6. Run tic-tac-toe game:
   ```
   $ sbt run
   ```

## Preview
```
The computer🤖 makes a decision...
The computer🤖 chose the move row = 1, column = 3
 x | x | o 
-----------
   | o |   
-----------
   |   |   
```

__Have fun! :)__

