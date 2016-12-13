package com.plteamscala.konane

class Player {
  /*
   * Default Constructor...
   */
  protected var board = new Board(8)
  protected var color = board.getDark
  protected var opponentColor = board.getLight
  protected var name = ""
  protected var numWins = 0

  /**
   * Constructor given a board and player color...
   */
  def this(board: Board, color: Int, name: String, numWins: Int) {
    this()
    this.board = board
    if (color == board.getDark || color == board.getLight)
      this.color = color
    if (color == board.getDark)
      opponentColor = board.getLight
    else
      opponentColor = board.getDark
    this.name = name
    this.numWins = numWins
  }

  def play(display: Boolean, web: Boolean, webChoice: Int): String = {
    ""
  }

  def play(display: Boolean): String = {
    ""
  }

  def setAutoplay(option: Boolean) {}

  def getName(): String = name

  def addWin() {
    numWins += 1
  }

  def getWins(): Int = numWins

  def newBoard(b: Board) {
    this.board = b
  }

  def getBoard(): Board = board
}