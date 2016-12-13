package com.plteamscala.konane

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable

class Board {
  /*
   * Default Constructor...
   */
  private var numEmpty = 0
  private var size = 8
  private var board = Array.ofDim[Int](size, size)

  // Dark goes first...
  // Might make sense to make this an enum...
  // Per spec, x for dark and o for light
  private val PLAYER_COLOR_DARK = 2
  private val PLAYER_COLOR_LIGHT = 1
  private val EMPTY_SPACE = 0

  // Getters
  def getSize(): Int = size
  def getNumEmpty(): Int = numEmpty
  def getLocation(x: Int, y: Int): Int = board(x)(y)
  def getDark(): Int = PLAYER_COLOR_DARK
  def getLight(): Int = PLAYER_COLOR_LIGHT

  /**
   * Constructor: Given a size
   */
  def this(size: Int) {
    this
    if (size <= 0)
      this.size = 0
    setupBoard
  }

  /**
   * Constructor: Copies a board
   */
  def this(b: Board) {
    this
    size = b.getSize
    numEmpty = b.getNumEmpty
    for (i <- 0 until size; j <- 0 until size)
      board(i)(j) = b.getLocation(i, j)
  }

  /**
   * Sets up the board for a Konane game
   */
  def setupBoard() {
    for (i <- 0 until size; j <- 0 until size)
      board(i)(j) = (i + j) % 2 + 1
  }

  /**
   * Returns the other player's int representation on the board.
   * Useful for finding valid moves...
   */
  def getOtherPlayer(player: Int): Int = {
    if (player == PLAYER_COLOR_DARK) {
      return PLAYER_COLOR_LIGHT
    }
    return PLAYER_COLOR_DARK
  }

  /**
   * Prints the board
   */
  def print() {
    var tmp = size
    printf(" ")
    for (i <- 0 until size)
      printf(" " + (i + 1))
    println("")
    for (i <- (size - 1) until -1 by -1) {
      for (j <- 0 until size) {
        if (j == 0) {
          printf(tmp + " ")
          tmp -= 1
        }
        if (board(i)(j) == PLAYER_COLOR_DARK) {
          printf("x ")
        } else if (board(i)(j) == PLAYER_COLOR_LIGHT) {
          printf("o ")
        } else if (board(i)(j) == EMPTY_SPACE) {
          printf(". ")
        }
      }
      println
    }
  }

  /**
   * Removes (first moves)
   */
  def remove(x: Int, y: Int, player: Int): Boolean = {
    if (validRemove(x, y, player)) {
      numEmpty += 1
      board(x)(y) = EMPTY_SPACE
      (true)
    } else {
      (false)
    }
  }

  /**
   * Ensures a remove is valid
   */
  def validRemove(x: Int, y: Int, player: Int): Boolean = {
    if (((x + y) % 2) + 1 != player)
      return false

    // In bounds?
    if (x < 0)
      return false
    if (y < 0)
      return false
    if (x >= size)
      return false
    if (y >= size)
      return false

    // First move?
    if (numEmpty == 0) {
      if ((x + y) != (size - 1))
        return false

      // Corner?
      if (x == 0)
        return true
      if (x == size - 1)
        return true
      // Middle?
      if (x == size / 2)
        return true
      if (x == (size - 1) / 2)
        return true
      // Otherwise
      return false
    }

    // Second move of game?
    if (numEmpty > 1)
      return false

    // Next to an empty?
    if ((x > 0) && (board(x - 1)(y) == 0))
      return true
    if ((y > 0) && (board(x)(y - 1) == 0))
      return true
    if ((x < size - 1) && (board(x + 1)(y) == 0))
      return true
    if ((y < size - 1) && (board(x)(y + 1) == 0))
      return true

    // Otherwise
    return false
  }

  /**
   * Standard move
   */
  def move(x1: Int, y1: Int, x2: Int, y2: Int, player: Int): Boolean = {
    if (validMove(x1, y1, x2, y2, player)) {
      var dy = (x1 - x2).abs
      var dx = (y1 - y2).abs
      if (dy > 2 || dx > 2) {
        for (x <- x1 to x2) {
          board(x)(y1) = EMPTY_SPACE
        }
        for (x <- x2 to x1) {
          board(x)(y1) = EMPTY_SPACE
        }
        for (y <- y1 to y2) {
          board(x1)(y) = EMPTY_SPACE
        }
        for (y <- y2 to y1) {
          board(x1)(y) = EMPTY_SPACE
        }
        board(x2)(y2) = player
        return true
      } else {
        board(x2)(y2) = player
        board(x1)(y1) = EMPTY_SPACE
        board((x1 + x2) / 2)((y1 + y2) / 2) = EMPTY_SPACE
        numEmpty += 1
        (true)
      }
    } else {
      (false)
    }
  }

  /**
   * Ensures a move is valid
   */
  def validMove(x1: Int, y1: Int, x2: Int, y2: Int, player: Int): Boolean = {
    // In bounds?
    if (x1 < 0)
      return false

    if (x1 >= size)
      return false

    if (x2 < 0)
      return false

    if (x2 >= size)
      return false

    if (y1 < 0)
      return false

    if (y1 >= size)
      return false

    if (y2 < 0)
      return false

    if (y2 >= size)
      return false

    // Start pieces?
    if (board(x1)(y1) != player)
      return false

    // Dest valid?
    if (board(x2)(y2) != EMPTY_SPACE)
      return false

    // Valid distance?
    var dy = (x1 - x2).abs
    var dx = (y1 - y2).abs

    // Multi jump
    if ((dy > 2 || dx > 2) && (dy % 2 == 0 || dx % 2 == 0)) {
      var other = getOtherPlayer(player)
      var flip = false
      if (dx == 0) {
        if (x1 > x2) {
          // Left
          for (i <- (x1 - 1) to x2 by -1) {
            if (!flip) {
              if (board(i)(y1) != other) {
                return false
              }
            } else {
              if (board(i)(y1) != EMPTY_SPACE) {
                return false
              }
            }
            flip = !flip
          }
          return true
        } else if (x2 > x1) {
          // Right
          for (i <- (x1 + 1) to x2) {
            if (!flip) {
              if (board(i)(y1) != other) {
                return false
              }
            } else {
              if (board(i)(y1) != EMPTY_SPACE) {
                return false
              }
            }
            flip = !flip
          }
          return true
        }
      }
      if (dy == 0) {
        if (y1 > y2) {
          // Down
          for (i <- (y1 - 1) to y2 by -1) {
            if (!flip) {
              if (board(x1)(i) != other) {
                return false
              }
            } else {
              if (board(x1)(i) != EMPTY_SPACE) {
                return false
              }
            }
            flip = !flip
          }
          return true
        } else if (y2 > y1) {
          // Up
          for (i <- (y1 + 1) to y2) {
            if (!flip) {
              if (board(x1)(i) != other) {
                return false
              }
            } else {
              if (board(x1)(i) != EMPTY_SPACE) {
                return false
              }
            }
            flip = !flip
          }
          return true
        }
      }
    }

    // Hop over?
    if (board((x1 + x2) / 2)((y1 + y2) / 2) == EMPTY_SPACE)
      return false

    if ((dx == 0) && (dy == 2))
      return true

    if ((dx == 2) && (dy == 0))
      return true

    // Otherwise
    return false
  }

  /**
   * Returns an ArrayBuffer of Tuples of possible valid moves for a given player.
   */
  def getPossibleMoves(playerColor: Int): ArrayBuffer[(Int, Int, Int, Int)] = {
    if (numEmpty < 2) {
      // Need to find removes rather than moves
      // tuple of x, y
      var removes = ArrayBuffer[(Int, Int, Int, Int)]()
      for (i <- 0 until size; j <- 0 until size) {
        if (validRemove(i, j, playerColor))
          removes += ((i, j, -1, -1))
      }
      return removes
    } else {
      // After removes, main game play...
      // tuple of startX, startY, endX, endY
      var moves = ArrayBuffer[(Int, Int, Int, Int)]()
      for (i <- 0 until size; j <- 0 until size) {
        for (k <- 0 until size) {
          if (validMove(i, j, i, k, playerColor)) {
            moves += ((i, j, i, k))
          }
          if (validMove(i, j, k, j, playerColor)) {
            moves += ((i, j, k, j))
          }
        }
      }
      return moves
    }
  }

  /**
   * Simple toString
   */
  override def toString = {
    "[Board: numEmpty=" + numEmpty + ", size=" + size + ", board=" + this.print + "]"
  }
}
