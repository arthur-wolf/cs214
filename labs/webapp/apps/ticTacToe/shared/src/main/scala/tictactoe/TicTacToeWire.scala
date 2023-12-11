package tictactoe

import ujson.*
import scala.util.{Failure, Success, Try}

import cs214.webapp.wires.*
import cs214.webapp.exceptions.DecodingException
import cs214.webapp.{AppWire, WireFormat, UserId}
import cs214.webapp.messages.EventResponse.Wire

object TicTacToeWire extends AppWire[TicTacToeEvent, TicTacToeView]:
  import TicTacToeEvent.*
  import TicTacToeView.*

  override object eventFormat extends WireFormat[TicTacToeEvent]:

  // Serializer for events
    override def encode(t: TicTacToeEvent): Value =
      t match
        case Move(x, y) => 
          ujson.Obj(
            "eventType" -> "move", 
            "x" -> x, 
            "y" -> y)

    // Deserializer for events
    override def decode(json: Value): Try[TicTacToeEvent] = 
      def loop(json : Value): TicTacToeEvent =
        val obj = json.obj
        val event = obj("eventType").str
        event match
          case "move" => Move(obj("x").num.toInt, obj("y").num.toInt)
          case _ => throw new DecodingException("Unknown event type")
      Try(loop(json))
      
  override object viewFormat extends WireFormat[TicTacToeView]:

    // Serializer for views : View -> JSON
    def encode(t: TicTacToeView): Value =
      t match
        case Finished(winner) => 
          ujson.Obj(
            "viewType" -> "Finished", 
            "winner" -> winner.map(_.toString).getOrElse("None")
            )
        case Playing(board, yourTurn) => 
          ujson.Obj(
            "viewType" -> "Playing", 
            "board" -> serializeBoard(board),
            "yourTurn" -> yourTurn)

    // Deserializer for views : JSON -> View
    def decode(json: Value): Try[TicTacToeView] =
      Try {
        val obj = json.obj
        val viewType = obj("viewType").str
        viewType match
          case "Finished" => 
            val winner = obj("winner").str match
              case "None" => None
              case userId => Some(userId)
            
            Finished(winner)
          case "Playing" => 
            val board = deserializeBoard(obj("board"))
            val yourTurn = obj("yourTurn").bool
            Playing(board, yourTurn)
          case _ => throw new DecodingException("Unknown view type")
      }

    def serializeBoard(board: Board): Value = {
      val cellsArray = for {
        row <- 0 until 3
        col <- 0 until 3
        cell = board.cells(row)(col)
      } yield ujson.Obj(
        "x" -> row,
        "y" -> col,
        "player" -> {
          cell match
            case None => ujson.Null
            case Some(userId) => ujson.Str(userId)
          } 
      )
      ujson.Arr(cellsArray: _*)
    }

    def deserializeBoard(json: Value): Board = {
      val cells = Array.fill(3, 3)(None: Option[UserId])
      json.arr.foreach { cellJson =>
        val x = cellJson("x").num.toInt
        val y = cellJson("y").num.toInt
        val player = 
          if (cellJson("player").isNull) None 
          else Some(cellJson("player").str)
        cells(x)(y) = player
      }
      Board(cells.toVector.map(_.toVector))
    }

