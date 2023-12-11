package memory

import scala.util.{Failure, Success, Try}

import cs214.webapp.*
import cs214.webapp.wires.*
import cs214.webapp.exceptions.DecodingException

object MemoryWire extends AppWire[MemoryEvent, MemoryView]:
  import MemoryEvent.*
  import MemoryView.*
  import ujson.*

  override object eventFormat extends WireFormat[MemoryEvent]:
    override def encode(event: MemoryEvent): Value =
      Obj(
        "type" -> Str("MemoryEvent"),
        "content" -> {
          event match
            case Toggle(cardId) =>
              Obj(
                "type" -> "Toggle",
                "cardId" -> Num(cardId)
              )
            case FlipSelected =>
              Obj(
                "type" -> "FlipSelected"
              )
        }
      )

    override def decode(js: Value): Try[MemoryEvent] =
      Try{
        js match
          case Obj(value) =>
            require(value("type") == Str("MemoryEvent"))
            val content = value("content").obj
            val eventType = content("type").str
            eventType match
              case "Toggle" =>
                val cardId = content("cardId").num.toInt
                Toggle(cardId)
              case "FlipSelected" =>
                FlipSelected
              case _ => throw DecodingException("Invalid event type")
          case _ => throw DecodingException("Invalid event type")
      }

  override object viewFormat extends WireFormat[MemoryView]:

    override def encode(v: MemoryView): Value =
      v match
        case MemoryView(stateView, alreadyMatched) =>
          Obj(
            "type" -> Str("MemoryView"),
            "content" -> {
              stateView match
                case StateView.Playing(phase, currentPlayer, board) =>
                  Obj(
                    "type" -> Str("Playing"),
                    "phase" -> {phase match
                      case PhaseView.SelectingCards =>
                        Str("SelectingCards")
                      case PhaseView.CardsSelected =>
                        Str("CardsSelected")
                      case PhaseView.Waiting =>
                        Str("Waiting")
                      case PhaseView.GoodMatch =>
                        Str("GoodMatch")
                      case PhaseView.BadMatch =>
                        Str("BadMatch")
                    },
                    "currentPlayer" -> Str(currentPlayer),
                    "board" -> { Arr.from(board.map(cardView => {
                      cardView match
                        case CardView.FaceDown =>
                            Str("FaceDown")
                        case CardView.Selected =>
                            Str("Selected")
                        case CardView.FaceUp(card) =>
                            Obj(
                              "type" -> Str("FaceUp"),
                              "card" -> Arr(Str(card))
                            )
                        case CardView.AlreadyMatched(card) =>
                            Obj(
                              "type" -> Str("AlreadyMatched"),
                              "card" -> Arr(Str(card))
                            )
                    }))
                  })
                case StateView.Finished(winnerIds) =>
                  Obj(
                    "type" -> Str("Finished"),
                    "winnerIds" -> SetWire(StringWire).encode(winnerIds)
                  )
              
            },
            "alreadyMatched" -> MapWire(StringWire, SeqWire(StringWire)).encode(alreadyMatched)
          )
          
    override def decode(js: Value): Try[MemoryView] =
      Try{
        js match
          case Obj(value) =>
            if value("type") != Str("MemoryView") then throw DecodingException("Invalid view type")
            val content = value("content").obj
            require(content.contains("type"))
            val viewType = content("type")
            val stateView = {
              viewType match
                case Str("Playing") =>
                  val phase = content("phase") match
                    case Str("SelectingCards") => PhaseView.SelectingCards
                    case Str("CardsSelected") => PhaseView.CardsSelected
                    case Str("Waiting") => PhaseView.Waiting
                    case Str("GoodMatch") => PhaseView.GoodMatch
                    case Str("BadMatch") => PhaseView.BadMatch
                    case _ => throw DecodingException("Invalid phase type")
                  val currentPlayer = content("currentPlayer").str
                  val board = content("board").arr.map(cardView => {
                    cardView match
                      case Str("FaceDown") =>
                        CardView.FaceDown
                      case Str("Selected") =>
                        CardView.Selected
                      case Obj(value) => {
                        if value ("type") == Str("FaceUp") then
                          val card = value("card").arr.map(StringWire.decode(_).get).head
                          CardView.FaceUp(card)
                        else if value ("type") == Str("AlreadyMatched") then
                          val card = value("card").arr.map(StringWire.decode(_).get).head
                          CardView.AlreadyMatched(card)
                        else throw DecodingException("Invalid card view type")
                      }
                      case _ => throw DecodingException("Invalid card view type")
                  }).toSeq
                  StateView.Playing(phase, currentPlayer, board)
                case Str("Finished") =>
                  val winnerIds = SetWire(StringWire).decode(content("winnerIds")).get
                  StateView.Finished(winnerIds)
                case _ => throw DecodingException("Invalid view type")
            }
            val alreadyMatched = MapWire(StringWire, SeqWire(StringWire)).decode(value("alreadyMatched")).get
            MemoryView(stateView, alreadyMatched)
          case _ => throw DecodingException("Invalid view type")
      }