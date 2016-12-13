package com.plteamscala.konane

import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable

class MinimaxPlayer(board: Board, color: Int, name: String, numWins: Int) extends Player(board: Board, color: Int, name: String, numWins: Int) {
  /*
   * Default Constructor...
   */
  private var bestMoveIndex = 0
  private var CUTOFF_DEPTH = 4
  val r = scala.util.Random
  var heuristics = 0
  var branchingFactor = 0
  var cutOffs = 0

  /**
   * Returns the other player's int representation on the board.
   * Useful for minimax search...
   */
  def getOtherPlayer(player: Int): Int = {
    if (player == board.getDark) {
      return board.getLight
    }
    return board.getDark
  }

  /*
   * Minimax Function:
   * Recursively searches possible moves to find the best move for
   * the maximizing player, and the worst for the minimizing player.
   *
   * To find these moves, this function uses the heuristic of
   * possibleMovesForMaxPlayer - possibleMovesForMinPlayer.
   *
   * Performs alpha beta pruning and executes minimax algorithm
   */
  def minimax(state: Board, depth: Int, who: Int, a: Int, b: Int): Int = {
    var alpha = a
    var beta = b

    // Base cases...
    if (depth >= CUTOFF_DEPTH) {
      // Depth limit reached, return the moves heuristical value...
      heuristics += 1
      cutOffs += 1
      return (state.getPossibleMoves(who).length - state.getPossibleMoves(getOtherPlayer(who)).length)
    }
    if (state.getPossibleMoves(who).length == 0) {
      // End game state reached, return the integer representation of who would win...
      heuristics += 1
      getOtherPlayer(who)
    }

    // Recursive search...
    if (who == color) {
      var max = Int.MinValue
      var temp = max
      var moves = state.getPossibleMoves(who)
      branchingFactor += moves.length
      breakable {
        for (i <- 0 until moves.length) {
          var tmp = new Board(state)
          if (state.getNumEmpty < 2) {
            tmp.remove(moves(i)._1, moves(i)._2, who)
          } else {
            tmp.move(moves(i)._1, moves(i)._2, moves(i)._3, moves(i)._4, who)
          }
          max = Math.max(max, minimax(tmp, (depth + 1), getOtherPlayer(who), alpha, beta))
          alpha = Math.max(max, a)
          if (temp != max && depth == 0) {
            bestMoveIndex = i
            temp = max
          }
          if (beta <= alpha) {
            cutOffs += 1
            break
          }
        }
      }
      return max
    } else {
      var min = Int.MaxValue
      var moves = state.getPossibleMoves(who)
      branchingFactor += moves.length
      breakable {
        for (i <- 0 until moves.length) {
          var tmp = new Board(state)
          if (state.getNumEmpty < 2) {
            tmp.remove(moves(i)._1, moves(i)._2, who)
          } else {
            tmp.move(moves(i)._1, moves(i)._2, moves(i)._3, moves(i)._4, who)
          }
          min = Math.min(min, minimax(tmp, (depth + 1), getOtherPlayer(who), alpha, beta))
          beta = Math.min(min, b)
          if (beta <= alpha) {
            cutOffs += 1
            break
          }
        }
      }
      return min
    }
  }

  /**
   * Begins play for the minimax player
   */
  override def play(display: Boolean, web: Boolean, webChoice: Int): String = {
    // Initializes statistics for this move
    heuristics = 0
    branchingFactor = 0
    cutOffs = 0
    bestMoveIndex = 0
    // Returns list of possible moves
    var moves = board.getPossibleMoves(color)
    println("\nMM is deciding between " + (moves.length - 1) + " possible moves...")
    if (moves.length == 0) {
      return ""
    } else {
      minimax(board, 0, color, Int.MinValue, Int.MaxValue)
      if (board.getNumEmpty < 2) {
        board.remove(moves(bestMoveIndex)_1, moves(bestMoveIndex)_2, color)
        println("remove <" + (moves(bestMoveIndex)._1 + 1) + ", " + (moves(bestMoveIndex)._2 + 1) + ">")
        println("  Move Details:")
        println("    Heuristics: " + heuristics)
        println("    Branching Factor: " + (branchingFactor / 4))
        println("    Cut Offs: " + cutOffs)
        return ("{\"from\": \"scala\", \"type\": \"remove\", \"x\": " + (moves(bestMoveIndex)._1 + 1) + ", \"y\": " + (moves(bestMoveIndex)._2 + 1) + "}")
      } else {
        board.move(moves(bestMoveIndex)_1, moves(bestMoveIndex)_2, moves(bestMoveIndex)_3, moves(bestMoveIndex)_4, color)
        println("move <" + (moves(bestMoveIndex)._1 + 1) + ", " + (moves(bestMoveIndex)._2 + 1) + "> to <" + (moves(bestMoveIndex)._3 + 1) + ", " + (moves(bestMoveIndex)._4 + 1) + ">")
        println("  Move Details:")
        println("    Heuristics: " + heuristics)
        println("    Branching Factor: " + (branchingFactor / 4))
        println("    Cut Offs: " + cutOffs)
        return ("{\"from\": \"scala\", \"type\": \"move\", \"x1\": " + (moves(bestMoveIndex)._1 + 1) + ", \"y1\": " + (moves(bestMoveIndex)._2 + 1) + ", \"x2\": " + (moves(bestMoveIndex)._3 + 1) + ", \"y2\": " + (moves(bestMoveIndex)._4 + 1) + "}")
      }
    }
  }
}
