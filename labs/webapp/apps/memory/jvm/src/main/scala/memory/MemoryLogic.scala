package memory

import scala.util.{Try, Random}

import ujson.Value

import cs214.webapp.*
import cs214.webapp.messages.*
import cs214.webapp.exceptions.*
import cs214.webapp.server.WebServer

import memory.*

// Feel free to tweak this value!
private val SHOW_CARDS_PAUSE_MS = 2500

object MemoryStateMachine extends cs214.webapp.StateMachine[MemoryEvent, MemoryState, MemoryView]:

  val name: String = "memory"
  val wire = MemoryWire

  def Deck(cards: String): Vector[String] =
    cards.trim.split(" +").to(Vector)

  val DECKS: Map[String, Vector[String]] = Map(
    "Simple" -> Deck("""
      💫 ⭐️
    """),
    "Stars" -> Deck("""
      💫 ⭐️ 🌟 ✨ ☀️
    """),
    "Animals" -> Deck("""
      🐵 🐒 🦍 🦧 🐶 🐕 🦮 🐕‍🦺
      🐩 🐺 🦊 🦝 🐱 🐈 🐈‍⬛ 🦁
      🐯 🐅 🐆 🐴 🫎 🫏 🐎 🦄
      🦓 🦌 🦬 🐮 🐂 🐃 🐄 🐷
      🐖 🐗 🐽 🐏 🐑 🐐 🐪 🐫
      🦙 🦒 🐘 🦣 🦏 🦛 🐭 🐁
      🐀 🐹 🐰 🐇 🐿️ 🦫 🦔 🦇
      🐻 🐻‍❄️ 🐨 🐼 🦥 🦦 🦨 🦘
      🦡
    """),
    "Birds" -> Deck("""
      🦃 🐔 🐓 🐣 🐤 🐥 🐦 🐧
      🕊️ 🦅 🦆 🦢 🦉 🦤 🪶 🦩
      🦚 🦜 🪽 🐦‍⬛ 🪿
    """),
    "Marine & Reptiles" -> Deck("""
      🐸 🐊 🐢 🦎 🐍 🐲 🐉 🦕
      🦖 🐳 🐋 🐬 🦭 🐟 🐠 🐡
      🦈 🐙 🐚 🪸 🪼 🦀 🦞 🦐
      🦑 🦪
    """),
    "Bugs" -> Deck("""
      🐌 🦋 🐛 🐜 🐝 🪲 🐞 🦗
      🪳 🕷️ 🕸️ 🦂 🦟 🪰 🪱 🦠
    """),
    "Plants" -> Deck("""
      💐 🌸 💮 🪷 🏵️ 🌹 🥀 🌺
      🌻 🌼 🌷 🪻 🌱 🪴 🌲 🌳
      🌴 🌵 🌾 🌿 ☘️ 🍀 🍁 🍂
      🍃 🍄 🪨 🪵
    """)
  )

  // Use any strings you want here — the tests don't check for these specific emoji
  val CARDS: Vector[String] = DECKS("Birds")

  /** Creates a new application state. */
  override def init(clients: Seq[UserId]): MemoryState =
    val shuffled = Random.shuffle(CARDS ++ CARDS)
    val board = shuffled.zipWithIndex
    val users = clients
    val score = users.map(_ -> Seq[String]()).toMap
    State(shuffled.map(that => (that, CardView.FaceDown)), (None, None), false, false, 0, true, users, score)

  override def transition(state: MemoryState)(userId: UserId, event: MemoryEvent): Try[Seq[Action[MemoryState]]] =
    Try{
      if state.players(state.currentPlayer) != userId then throw IllegalMoveException("Not your turn")
      event match
        case  MemoryEvent.Toggle(cardId) =>
          if cardId < 0 || cardId >= state.state.length then throw IllegalMoveException("Invalid card Id")
          if state.state(cardId)._2 == CardView.AlreadyMatched(state.state(cardId)._1) then throw IllegalMoveException("Already matched card")
          var post: Option[Vector[(Card, CardView)]] = None
          val newSel = {
            state.selCardsId match 
              case (None, None) => 
                post = Some(state.state.updated(cardId, (state.state(cardId)._1, CardView.Selected)))
                (Some(cardId), None)
              case (Some(card1), card2) if card1 == cardId => 
                post = Some(state.state.updated(cardId, (state.state(cardId)._1, CardView.FaceDown)))
                (card2, None)
              case (card1, Some(card2)) if card2 == cardId => 
                post = Some(state.state.updated(cardId, (state.state(cardId)._1, CardView.FaceDown)))
                (card1, None)
              case (Some(card1), None) => 
                post = Some(state.state.updated(cardId, (state.state(cardId)._1, CardView.Selected)))
                (Some(card1), Some(cardId))
              case (None, Some(card2)) => 
                post = Some(state.state.updated(cardId, (state.state(cardId)._1, CardView.Selected)))
                (Some(card2), Some(cardId))
              case (Some(_), Some(_)) => throw IllegalMoveException("Impossible to select more than two cards")
          }
          val tempBool = false
          Seq(Action.Render(State(post.get, newSel, tempBool, tempBool, state.currentPlayer, !tempBool, state.players, state.score)))
        
        case MemoryEvent.FlipSelected =>
          state.selCardsId match
            case (Some(card1), Some(card2)) => 
              val newBoard = state.state
                              .updated(card1, (state.state(card1)._1, CardView.FaceUp(state.state(card1)._1)))
                              .updated(card2, (state.state(card2)._1, CardView.FaceUp(state.state(card2)._1)))

              val (nextState, nextPlayer, finish, matched, newScore) = {
                (state.state(card1), state.state(card2)) match
                  case ((x, xs), (y, ys)) if x == y => 
                    val nextBoard = state.state
                      .updated(card1, (state.state(card1)._1, CardView.AlreadyMatched(state.state(card1)._1)))
                      .updated(card2, (state.state(card2)._1, CardView.AlreadyMatched(state.state(card2)._1)))
                    val isFull = nextBoard.forall(that => {
                      val (card, view) = that
                      view match {
                        case CardView.AlreadyMatched(_) => true
                        case _ => false
                      }
                    })
                    val newScore = state.score.updated(userId, state.score(userId) ++ Seq(x, y))
                    (nextBoard, state.currentPlayer, isFull, true, newScore)
                  case ((x, xs), (y, ys)) =>
                    val nextBoard = state.state
                      .updated(card1, (state.state(card1)._1, CardView.FaceDown))
                      .updated(card2, (state.state(card2)._1, CardView.FaceDown))
                    (nextBoard, (state.currentPlayer + 1) % state.players.length, false, false, state.score)
              }
              val tempBool = true
              Seq(Action.Render(State(newBoard, (Some(card1), Some(card2)), tempBool, matched, nextPlayer, tempBool, state.players, newScore)), 
                Action.Pause(SHOW_CARDS_PAUSE_MS),
                Action.Render(State(nextState, (None, None), !tempBool, !tempBool, nextPlayer, !finish, state.players, newScore))
                )

            case _ => throw IllegalMoveException("Impossible to flip cards if not selected")
              }

  override def project(state: MemoryState)(userId: UserId): MemoryView =
    if state.playing then
      val currentPhase = {
        if (userId != state.players(state.currentPlayer) && !state.showSelCards) then 
          PhaseView.Waiting
        else if ((state.selCardsId._1.isEmpty || state.selCardsId._2.isEmpty) && !state.showSelCards) then
          PhaseView.SelectingCards
        else if (!(state.selCardsId._1.isEmpty && state.selCardsId._2.isEmpty) && !state.showSelCards) then
          PhaseView.CardsSelected
        else if (state.showSelCards) then 
          PhaseView.GoodMatch
        else 
          PhaseView.BadMatch
      }
      MemoryView(StateView.Playing(currentPhase, state.players(state.currentPlayer), state.state.map(_._2)), state.score)
    else
      val winner = state.score.toSeq.groupBy(_._2.size).toSeq.sortBy(_._1).last._2.map(_._1).toSet
      MemoryView(StateView.Finished(winner), state.score)
      
      

// Server registration magic
class register:
  WebServer.register(MemoryStateMachine)
