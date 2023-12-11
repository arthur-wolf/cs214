package tictactoe

import scala.collection.immutable.Queue
import scala.util.{Failure, Success, Try}

import cs214.webapp.*
import cs214.webapp.exceptions.*
import cs214.webapp.server.WebServer
import cs214.webapp.messages.Action

object TicTacToeStateMachine extends cs214.webapp.StateMachine[TicTacToeEvent, TicTacToeState, TicTacToeView]:

  val name: String = "tictactoe"
  val wire = TicTacToeWire

  override def init(clients: Seq[UserId]): TicTacToeState =
    State.Playing(Board(), clients.last)

  // Failures in the Try must hold instances of AppException
  // (from Exceptions.scala under lib/shared/)
  override def transition(state: TicTacToeState)(uid: UserId, event: TicTacToeEvent): Try[Seq[Action[TicTacToeState]]] =
  state match {
    // Game is still in progress
    case State.Playing(board, unallowedPlayer) =>
      // Check if it's the user's turn
      if (unallowedPlayer == uid)
        Failure(new NotYourTurnException)
      else event match {
        // Move event
        case TicTacToeEvent.Move(x, y) =>
          if (x < 0 || x > 2 || y < 0 || y > 2)
            Failure(new IllegalMoveException("Coordinates out of bounds"))
          else board(x, y) match {
            // Cell is not occupied
            case None =>
              // Update board and check for winner or next state
              val newBoard = updateBoard(board, x, y, uid)

              if !newBoard.cells.exists(line => line.contains(None)) then
                Success(Seq(Action.Render(State.Finished(None))))
              else 
                val winner = checkForWinner(newBoard)
                winner match {
                  // swap the state to Finished since a winner is found
                  case Some(w) => Success(Seq(Action.Render(State.Finished(Some(w)))))
                  // swap the state to Playing with the updated board and the turn swapped
                  case None => Success(Seq(Action.Render(State.Playing(newBoard, uid))))
                }
            // Cell is occupied  
            case Some(_) => Failure(new IllegalMoveException("Cell is already occupied"))
          }
        // Invalid event -> Only Move is valid  
        case _ => Failure(new IllegalMoveException("Invalid event"))
      }
    // Game is already finished and we can't play anymore
    case State.Finished(_) => Failure(new IllegalMoveException("Game is already finished"))
  }

  override def project(state: TicTacToeState)(uid: UserId): TicTacToeView =
    state match {
      case State.Playing(board, unallowedPlayer) => TicTacToeView.Playing(board, unallowedPlayer != uid)
      case State.Finished(winner) => TicTacToeView.Finished(winner)
    }

  // Helper functions
  def updateBoard(board: Board, x: Int, y: Int, userId: UserId): Board = {
    require((0 <= x && x < 3) && (0 <= y && y < 3), "Coordinates out of bounds")
    require(board.cells(x)(y).isEmpty, "Cell is already occupied")

    val newBoard = board.cells.updated(x, board.cells(x).updated(y, Some(userId)))
    
    Board(newBoard)
  }

  def checkForWinner(board: Board): Option[UserId] = {
    var winner: Option[UserId] = None
    // Check rows and columns
    for (i <- 0 until 3) {
      if (board(i, 0).isDefined && board(i, 0) == board(i, 1) && board(i, 1) == board(i, 2))
        winner = board(i, 0)
      if (board(0, i).isDefined && board(0, i) == board(1, i) && board(1, i) == board(2, i))
        winner =  board(0, i)
    }

    // Check diagonals
    if (board(0, 0).isDefined && board(0, 0) == board(1, 1) && board(1, 1) == board(2, 2))
      winner = board(0, 0)
    if (board(0, 2).isDefined && board(0, 2) == board(1, 1) && board(1, 1) == board(2, 0))
      winner = board(0, 2)

    // No winner
    winner
  }

// Server registration magic
class register:
  WebServer.register(TicTacToeStateMachine)
