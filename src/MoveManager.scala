/**
 * Created by willsam100 on 9/02/14.
 */
object MoveManager {
  def deepCopy[A](array: Array[A])(implicit ev: Nested[Array[A]]): Array[A] = {
    ev.clone(array)
  }

  /*
   * Performs all moves on the game and returns the new game states
   */
  def performAllMoves(game: GameState): List[GameState] = {

    // Start with playing the deck
    val newGame = new GameState(game.deck, deepCopy(game.floorHidden), deepCopy(game.floorVisable))
    newGame.score = game.score
    newGame.history = game.history
    newGame.playDeck()

    var results = List(newGame)

    for (i <- 0 until game.floorVisable.size) {
      // find all the movable cards,

      val movableCards = findMovablecardInColumn(game.floorVisable(i), null)

      // check if the pile is empty
      if (movableCards.size > 0) {

        val newGames = moveMovableCards(movableCards, i, game)

        // Add all the games that are not already in the list
        results = newGames.filter(newGame => !gameExists(results, newGame)) ++ results
      }
    }

    sortGames(results)
  }

  def findMovablecardInColumn(column: List[Card], firstCard: Card): List[Card] = (column, firstCard) match {
    case (c, f) if c.isEmpty => List()
    case (c, null) if c.size == 1 => List(c.head)
    case (c, null) => c.head +: findMovablecardInColumn(c.tail, c.head)
    case (c, f) if c.head.getClass == firstCard.getClass && c.head.n == firstCard.n + 1 => c.head +: findMovablecardInColumn(c.tail, c.head)
    case _ => List()
  }


  /*
   * Attempts to move card to a new place
   * Also computes the score of the move
   */
  def moveMovableCards(moveableCards: List[Card], index: Int, game: GameState): List[GameState] = {

    var result = List[GameState]()

    // loop through the floor and see where the cards can be placed,
    for (column <- 0 until game.floorVisable.size) {
      if (column != index) {
        for (i <- moveableCards.size to 1 by -1) {
          val cards = moveableCards.take(i)
          if (game.floorVisable(column).size > 0 && game.floorVisable(column).head.n == cards.last.n + 1) {
            // the card can be moved. create a game

            val newGame = createNewGame(game, cards, column, index)

            result = if (gameExists(result, newGame)) result else result :+ newGame
          }
        }
      }
    }

    result
  }

  def createNewGame(game: GameState, cards: List[Card], column: Int, index: Int) = {
    val newGame = new GameState(game.deck, deepCopy(game.floorHidden), deepCopy(game.floorVisable))
    newGame.score = game.score
    newGame.history = game.history
    newGame.moveCard(index, cards.size, column)


    if (game.floorVisable(column).size > 0 && game.floorVisable(column).head.getClass == cards.head.getClass) {
      // Award 100 points x the size of the pile of the same suit
      newGame.score += awardScore(game.floorVisable(column), cards.last) + (cards.size * 1000)
    }
    // Empty pile the card can be moved, create a new game and update score
    else if (game.floorVisable(column).isEmpty && cards.last.n == 13) {

      println("Moved King to empty field")
      newGame.score += 500
    }

    // Check if they are moving the last cards
    else if (game.floorVisable(column).isEmpty) {
      println("Moved Card to empty field")
      newGame.score += 200
    }
    // Normal move, award 5 points
    else {
      newGame.score += ((6 - game.floorHidden(column).size) * 100) - (game.floorVisable(column).size * 40)
    }

    newGame
  }

  /*
  * Calculates 100 times the size of the colum where the the cards are the same suit
  */
  def awardScore(colum: List[Card], card: Card): Int = {
    val score = colum.foldLeft(0)((score, columnCard) => if (columnCard.getClass != card.getClass) return score else score + 1000)
    if (colum.size == 13)
      score * 2
    else
      score
  }

  def gameExists(results: List[GameState], newGame: GameState): Boolean = results.foldLeft(false)((value, game) =>
    if (game == newGame || value) true else false)


  /*
  * Comparator to sort two GameStates by score
  */
  def sortGames(games: List[GameState]) = {
    def s(e1: GameState, e2: GameState) = e1.score > e2.score
    games.sortWith(s)
  }

}

