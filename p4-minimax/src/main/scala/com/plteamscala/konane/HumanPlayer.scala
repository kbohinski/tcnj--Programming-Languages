package com.plteamscala.konane

class HumanPlayer(board: Board, color: Int, name: String, numWins: Int) extends Player(board: Board, color: Int, name: String, numWins: Int) {
  /* 
   * Default constructor
   */
  var autoplay = false
  val r = scala.util.Random
  var input = false

  /**
   * Setter for autoplay
   */
  override def setAutoplay(option: Boolean) {
    autoplay = option
  }

  /**
   * Begins game play for the human player
   * Could likely split this up into multiple functions for ease of updating and reading...
   */
  override def play(display: Boolean, web: Boolean, webChoice: Int): String = {
    var moves = board.getPossibleMoves(color)
    if (moves.length == 0) {
      if (display) {
        println("Sorry, you have no moves left!")
      }
      return ""
    }
    if (display) {
      println("\nHere are your availiable moves, select one:")
    }
    if (board.getNumEmpty < 2) {
      var choice = -1
      if (!web) {
        if (display) {
          for (i <- 0 until moves.length) {
            println(i + ") remove <" + (moves(i)._1 + 1) + ", " + (moves(i)._2 + 1) + ">")
          }
        }
        choice = r.nextInt(moves.length)
        /*
         * Performs error checking to ensure that the user cannot enter an invalid move
         */
        if (!autoplay) {
          while (!input) {
            choice = scala.io.StdIn.readInt()
            if (choice < moves.length && choice >= 0) {
              input = true
            } else {
              println("Sorry, that move is not valid. Try again.")
            }
          }
          input = false
        }
      } else {
        choice = webChoice
      }
      board.remove(moves(choice)._1, moves(choice)._2, color)
    } else {
      var choice = -1
      if (!web) {
        if (display) {
          for (i <- 0 until moves.length) {
            println(i + ") move <" + (moves(i)._1 + 1) + ", " + (moves(i)._2 + 1) + "> to <" + (moves(i)._3 + 1) + ", " + (moves(i)._4 + 1) + ">")
          }
        }
        choice = r.nextInt(moves.length)
        /*
         * Performs error checking to ensure that the user cannot enter an invalid move
         */
        if (!autoplay) {
          while (!input) {
            choice = scala.io.StdIn.readInt()
            if (choice < moves.length && choice >= 0) {
              input = true
            } else {
              println("Sorry, that move is not valid. Try again.")
            }
          }
          input = false
        }
      } else {
        choice = webChoice
      }
      println("Human takes the move with id: " + choice + " out of " + (moves.length - 1))
      board.move(moves(choice)._1, moves(choice)._2, moves(choice)._3, moves(choice)._4, color)
    }
    return "move taken"
  }
}
