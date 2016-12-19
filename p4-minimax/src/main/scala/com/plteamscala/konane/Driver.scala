package com.plteamscala.konane

import java.awt.Desktop
import java.io.File
import java.net.URI
import java.util.Arrays

import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import com.pubnub.api.PNConfiguration
import com.pubnub.api.PubNub
import com.pubnub.api.callbacks.SubscribeCallback
import com.pubnub.api.models.consumer.PNStatus
import com.pubnub.api.models.consumer.pubsub.PNMessageResult
import com.pubnub.api.models.consumer.pubsub.PNPresenceEventResult

object Driver {

  /**
   * Main method, where Scala starts...
   */
  def main(args: Array[String]) {

    /**
     * Setting up PubNub, and other vars for network operations
     */
    val pnConfig = new PNConfiguration()
    pnConfig.setPublishKey("pub-c-af9d3333-b289-4e2f-aeaa-e0373d09a97e")
    pnConfig.setSubscribeKey("sub-c-9140b13e-b802-11e6-b07a-0619f8945a4f")
    var pnChannel = "konane-web"

    val pubnub = new PubNub(pnConfig)
    var connected = false
    var reply = false
    var moveId = -1

    /**
     * Event handler for incoming messages
     * (status and presence are required but we dont use them...)
     */
    pubnub.addListener(new SubscribeCallback() {
      def status(pubnub: PubNub, status: PNStatus) {}
      def message(pubnub: PubNub, message: PNMessageResult) {
        var msg = message.getMessage()
        var mapper = new ObjectMapper() with ScalaObjectMapper
        mapper.registerModule(DefaultScalaModule)
        var map = mapper.convertValue[Map[String, String]](msg)
        // Ignore any messages from myself
        if (map.contains("from") && map("from") == "browser") {
          println("From browser: " + msg)
          if (map.contains("status") && map("status") == "start") {
            connected = true
          }
          if (map.contains("type") && map("type") == "remove") {
            moveId = map("id").toInt
            reply = true
          }
        }
      }
      def presence(pubnub: PubNub, presence: PNPresenceEventResult) {}
    })

    def centerPrint(line: String) {
      println((" " * ((80 - line.length) / 2)) + line)
    }

    /**
     * Simple anon func to convert a JSON string to a Java HashMap for the Java based PubNub library.
     * (not tested for all JSON strings and likely does not follow JSON spec...)
     */
    def jsonStringToHashMap(t: String): java.util.HashMap[java.lang.String, java.lang.String] = {
      var m = new java.util.HashMap[java.lang.String, java.lang.String]

      var parts = t.split(",")

      for (i <- 0 until parts.length) {
        var tmp = parts(i).replace("\"", "")
        tmp = tmp.replace("{", "")
        tmp = tmp.replace("}", "")
        var kv = tmp.split(":")
        m.put(kv(0).trim, kv(1).trim)
      }

      return m
    }

    println("=" * 80)
    centerPrint("Project 4: Konane")
    centerPrint("Kevin Bohinski & Brittany Plummer & Hannah Richman")
    centerPrint("CSC 435: Programming Languages")
    println("=" * 80)
    println("Note: It is recommended to use sbt when running this.")
    println("Ex  : sbt clean update run clean\n")

    println("A few quick questions first:\n")

    println("Would you like to use the realtime web interface?")
    println("(with the default settings)")
    println("1) No")
    println("2) Yes\n")
    var web = (scala.io.StdIn.readInt() == 2)

    var size = 8
    var display = false
    var numGames = 1.0
    var numGamesFull = numGames
    var dark = 2
    var autoplay = false

    if (!web) {
      connected = true

      println("What size board would you like to play on?")
      println("(default = 8)\n")
      size = scala.io.StdIn.readInt()

      println("Would you like the board displayed between moves?")
      println("1) No")
      println("2) Yes\n")
      display = (scala.io.StdIn.readInt() == 2)

      println("How many games would you like to play?")
      println("(default = 1)\n")
      numGames = scala.io.StdIn.readDouble()
      numGamesFull = numGames

      println("Who will play the dark player (x, going first)?")
      println("1) Human Player")
      println("2) Minimax Player\n")
      dark = scala.io.StdIn.readInt()

      println("Thanks, finally would you like to automatically play the human player (random)?")
      println("1) No")
      println("2) Yes\n")
      autoplay = (scala.io.StdIn.readInt() == 2)

      println("Ok! Let's play!\n")
    } else {
      if (Desktop.isDesktopSupported()) {
        Desktop.getDesktop().browse(new File("index.html").toURI())
      }

      println("Please enter the room id from the website:")
      var roomId = scala.io.StdIn.readInt()
      pnChannel += roomId
      /**
       * Start listening!
       */
      pubnub.subscribe.channels(Arrays.asList(pnChannel)).execute()
      println("\nNow press start on the site!")
    }

    var x = new Player
    var o = new Player

    /*
     * Main game loop...
     */
    while (numGames != 0) {

      // As there is an option for multiple games, this loop starts by resetting the board and players...
      var board = new Board(size)

      if (dark == 1) {
        x = new HumanPlayer(board, board.getDark, "Human", x.getWins)
        o = new MinimaxPlayer(board, board.getLight, "MM", o.getWins)
      } else {
        x = new MinimaxPlayer(board, board.getDark, "MM", x.getWins)
        o = new HumanPlayer(board, board.getLight, "Human", o.getWins)
      }

      if (autoplay) {
        if (dark == 1) {
          x.setAutoplay(true)
        } else {
          o.setAutoplay(true)
        }
      }

      // Do not start the game until the user is ready...
      if (web) {
        while (!connected) {
          Thread.sleep(1000)
        }
        pubnub.publish().message(jsonStringToHashMap("{\"from\": \"scala\", \"status\": \"connected\"}")).channel(pnChannel).sync
      }

      // Core game loop...
      breakable {
        while (true) {
          if (display) {
            println("")
            println("X (" + x.getName + ")'s turn:")
            board.print
            println("")
          }
          var x_move = x.play(display, web, moveId)
          if (x_move != "") {
            if (web && x.getName == "MM") {
              pubnub.publish().message(jsonStringToHashMap(x_move)).channel(pnChannel).sync
              println("Sent MM's move...")
            }
            if (display) {
              println("")
              println("O (" + o.getName + ")'s turn:")
              board.print
            }
            if (web) {
              var moves = board.getPossibleMoves(board.getLight)
              var json = "{\"from\": \"scala\", \"type\": \"options\""
              for (i <- 0 until moves.length) {
                if (i == 0) {
                  json += ","
                }
                json += "\"" + i + "\":"
                if (board.getNumEmpty < 2) {
                  json += "\"" + "remove <" + (moves(i)._1 + 1) + " " + (moves(i)._2 + 1) + ">" + "\""
                } else {
                  json += "\"" + "move <" + (moves(i)._1 + 1) + " " + (moves(i)._2 + 1) + "> to <" + (moves(i)._3 + 1) + " " + (moves(i)._4 + 1) + ">" + "\""
                }
                if (i != moves.length - 1) {
                  json += ","
                }
              }
              json += "}"
              println(json)
              pubnub.publish().message(jsonStringToHashMap(json)).channel(pnChannel).sync
              println("Sent browser their possible moves...")
            }
            while (!reply && web) {
              Thread.sleep(250)
            }
            reply = false
            if (web) {
              println("Heard back from browser... they would like move " + moveId + " out of " + (board.getPossibleMoves(board.getLight).length - 1))
            }
            var o_move = o.play(display, web, moveId)
            if (o_move == "") {
              println("X (" + x.getName + ") wins!")
              x.addWin
              if (web) {
                pubnub.publish().message(jsonStringToHashMap("{\"type\": \"win\", \"from\": \"scala\", \"winner\": \"x\"}")).channel(pnChannel).sync
              }
              break
            }
          } else {
            println("O (" + o.getName + ") wins!")
            o.addWin
            if (web) {
              pubnub.publish().message(jsonStringToHashMap("{\"type\": \"win\", \"from\": \"scala\", \"winner\": \"o\"}")).channel(pnChannel).sync
            }
            break
          }
        }
      }

      numGames -= 1
    }

    // Prints some stats...
    println("\nLooks like all the games are over, here are the stats:")
    println("X (" + x.getName + ") won " + ((x.getWins / numGamesFull) * 100).toInt + "% (" + x.getWins + ") of the time.")
    println("O (" + o.getName + ") won " + ((o.getWins / numGamesFull) * 100).toInt + "% (" + o.getWins + ") of the time.")
    println("\nPlease exit via ctrl+c, for some reason the network operations prevent scala from exiting...")
  }
}
