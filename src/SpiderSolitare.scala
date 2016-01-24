/**
 * Created by willsam100 on 8/02/14.
 */

sealed class Card(val n: Int) {
  if(n <= 0 || n > 13) {
    throw new IllegalArgumentException("the number must be non-negative.")
  }
}

sealed case class HeartCard(num: Int) extends Card(num) {
  override def toString() = "Heart " + num
}
sealed case class DiamondCard(num: Int) extends Card(num) {
  override def toString() = "Diamond " + num
}
sealed case class SpadeCard(num: Int) extends Card(num) {
  override def toString() = "Spade " + num
}
sealed case class ClubsCard(num: Int) extends Card(num) {
  override def toString() = "Club " + num
}


object SpiderSolitare {

  var depth = 0

  def main(args: Array[String]) {

    val game = createFirstGame



    println(game.fullString)
    println("Starting game...")
    println(game)

    val result = playDepthRun(game, List[GameState]())

    println(result.head)
    println("GAME COMPLETED")


    //    game.moveCard(5,1,2)

    //    println(game)

    //    val game2 = MoveManager.performAllMoves(game).drop(1).head




    //    var allGames = List(MoveManager.performAllMoves(game))
    //
    //    for (i <- 1 until 5) {
    //
    //      val newGames = playGames(allGames.head).filter(newGame => !MoveManager.gameExists(allGames.head, newGame))
    //      allGames = if (newGames.isEmpty) allGames else newGames +: allGames
    //    }

    //    val allGames = playRun(List(game), List[List[GameState]]())


    //    allGames.reverse.foreach(a => {
    //      println("size", a.size)
    //      a.takeRight(10).foreach(g => {
    //        println(g)
    //        println(pad("M: " + g.history.take(1) + " score: " + g.score, 150, "-"))
    //        println()
    //        println()
    //      })
    //    })


    //    val heighestScore = allGames.head.foldLeft(0)((max: Int, game: GameState) => if (game.score > max) game.score else max)
    //    println("Heighest score", heighestScore)
    //    println()
    //
    //    val gamesSorted = sortGames(allGames.head)
    //
    //    gamesSorted.reverse.takeRight(3).foreach(g => {
    //      println(g)
    //      println(pad("M: " + g.history.head + " score: " + g.score, 150, "-"))
    //      println()
    //      println()
    //    })
    //
    //
    //        println(gamesSorted.head.history)

  }




  /*
   * Helper methods
   */
  /* ------------------------------------------------------------------------------------------------*/
  /* ------------------------------------------------------------------------------------------------*/
  /* ------------------------------------------------------------------------------------------------*/


  /*
   * play a run of six games a peice and then take take the five best games
   */
//  def playRun(games: List[GameState], all: List[List[GameState]]): List[List[GameState]] = {
//    println("Playing run", games.head.score)
//
//    var allGames = games +: all
//
//    for (i <- 1 until 3) {
//
//      val newGames = playGames(allGames.head).filter(newGame => !MoveManager.gameExists(allGames.flatten, newGame))
//
//      allGames = if (allGames.isEmpty) newGames +: allGames else if (allGames.head.isEmpty && newGames.isEmpty) allGames else newGames +: allGames
//
//    }
//
//    // There were no more moves
//    if (allGames.isEmpty) {
//      println("First game no moves, this is a bug")
//      return allGames
//    } else if (allGames.head.isEmpty) {
//
//      println("No more moves")
//      allGames.foreach(g => println(g.size))
//
//      // Remove the empty list at the head
//      return  allGames.tail
//    }
//
//    val sortedGames = sortGames(allGames.head)
//
//    val gamesToKeepPlaying = sortedGames.dropRight((sortedGames.size * 0.1).toInt)
//
//    //    val exitEarly = sortedGames.foldLeft(true)((result, game) => if (game.empyFloorVisible) result else false )
//    val exitEarly = sortedGames.foldLeft(false)((result, game) => if (game.score > 130000) return allGames else result)
//
//    if (exitEarly) {
//      println("exit early result found")
//      return allGames
//    }
//
//    gamesToKeepPlaying.take(3).reverse.foreach(g => {
//      println(g)
//      println(pad("M: " + g.history.take(4) + " score: " + g.score, 150, "-"))
//      println()
//      println()
//    })
//
//    println("Number of games generated", sortedGames.size);
//
//
//    playRun(gamesToKeepPlaying, allGames)
//  }

  /*
   * Depth first search
   */
  def playDepthRun(root: GameState, all: List[GameState]): List[GameState] = {
    depth = if (all.size > depth) all.size else depth
    var allGames = all
    val games = MoveManager.performAllMoves(root).filter(g => !MoveManager.gameExists(all, g))
//    if (allGames.size % 20 == 0) {
      println("depth:", all.size)
      println("max depth:", all.size)
      println(root)
      println(pad("M: " + root.history.take(5) + " score: " + root.score  + " cards: " + root.cardCount, 150, "-"))
      println()
      println()
//    }

    for (game <- games) {
      if (game.gameComplete) {

        return game +: allGames
      }
      else {
        if (!MoveManager.gameExists(allGames, game)) {

          if (all.size <= depth * 0.65) {
            println("depth:", all.size)
            println("max depth:", all.size)
            println(root)
            println(pad("M: " + root.history.take(5) + " score: " + root.score + " cards: " + root.cardCount, 150, "-"))
            println()
            println()
          }
          val result = playDepthRun(game, game +: all)
          if (result.head != game) {
            return result
          } else {
            allGames ++ result.filter(g => !MoveManager.gameExists(all, g))
          }

        }
      }
    }

    root +: (games ++ all)
  }


  /*
   * Comparator to sort two GameStates by score
   */
  def sortGames(games: List[GameState]) = {
    def s(e1: GameState, e2: GameState) = e1.score > e2.score
    games.sortWith(s)
  }


  /*
   * Recusive method to play all games for a set of games, and merge them back together.
   */
  def playGames(games: List[GameState]): List[GameState] = {
    var gameResults = List[List[GameState]]()
    for (game <- games) {

      val newGames = MoveManager.performAllMoves(game)
      gameResults = newGames.filter(newGame => !MoveManager.gameExists(gameResults.flatten, newGame)) +: gameResults

    }

    gameResults.flatten
  }

  /*
   * Generate the game deck
   */
  def generateDeck: List[Card] = {
    var deck = List[Card]();

    for (suit <- 1 to 8) {
      for (num <- 1 to 13) {
        deck = deck :+ buildCard(0, num)
      }
    }

    deck
  }

  /*
   * create a card for a given suit
   */
  def buildCard(suit: Int, num: Int): Card = suit match {
    case 1 => HeartCard(num)
    case 2 => DiamondCard(num)
    case 3 => SpadeCard(num)
    case 0 => ClubsCard(num)
  }

  /*
   * Shuffles the deck
   */
  def shuffleDeck(stock: List[Card]): List[Card] = {

    def inner(stock: List[Card], shuffled: List[Card]): List[Card] = (stock, shuffled) match {
      case (null, shuff) => shuff
      case (cards, shuff) if cards.size == 1 => cards.head +: shuff
      case (cards, shuff) => {
        val index = scala.util.Random.nextInt(cards.size)
        inner(cards.take(index) ++ cards.drop(index+1), cards(index) +: shuff)
      }
    }

    inner(stock, List[Card]())
  }



  /*
   * Intensice CPU task, don't use with more than 8 items
   */
  def buildAllPermutations(stock: List[Any]): Array[List[Any]] = stock match {
    case stock if stock.size == 0 => Array(List[Any]())
    case _ => {
      var returnValue = Array[List[Any]]()

      val decks = buildAllPermutations(stock.tail)
      for (deck <- decks) {
        for (i <- 0 to deck.size) {
          returnValue = returnValue :+ ((deck.take(i) :+ stock.head) ++ deck.drop(i))
        }
      }

      returnValue
    }
  }


  /*
   * Builds the first game required to start play
   */
  def createFirstGame: GameState = {
    val deck = shuffleDeck(generateDeck)

    var h = Array[List[Card]]()
    for (n <- 1 to 4) {
      h = h :+ deck.drop(50).take(44).drop(h.size).take(4)
    }

    for (n <- 1 to 6) {
      h = h :+ deck.drop(50).take(44).drop(h.size).take(3)
    }

    var v = Array[List[Card]]()
    for (n <- 1 to 10) {
      v = v :+ deck.drop(50 + 44).drop(v.size).take(1)
    }

    new GameState(deck.take(50), h, v)
  }


  /*
   * To pad a string in the center
   */
  def pad(s:String, width:Int, ch:String) = {
    val l = s.length
    val left = (width - l) / 2
    val right = width - left - l
    ch*left + s + ch*right
  }
}









