/**
 * Created by willsam100 on 8/02/14.
 */
class GameState(var deck: List[Card], var floorHidden: Array[List[Card]], var floorVisable: Array[List[Card]]) {

  var history = List[(Int, Int, Int)]()

  var cardCount = 104

  // Sanity checks
  require(deck.size <= 50)
  require(floorHidden.size <= 12)
  require(floorVisable.size <= 12)

  var score = 0

  def isLastCard(column: Int) = if (floorHidden(column).size > 0) true else false

  /*
   * Moves the card, does not do logic checking
   */
  def moveCard(from: Int, count: Int, to: Int) {

    // Sanity check
    if (floorVisable.size == 0) {
      return
    }
    history = (from, count, to) +: history

    // Move the group of cards
    floorVisable(to) =  floorVisable(from).take(count) ++ floorVisable(to)
    floorVisable(from) = floorVisable(from).drop(count)

    val set = floorVisable(to).take(13).foldLeft(1)((cardValue, card) => if (card.n == cardValue) cardValue + 1 else -1)

    if (set == 14) {
      println("Removed set of cards from game")
      println(this)
      println()

      floorVisable(to) = floorVisable(to).drop(13)
      cardCount -= 13

//      println(this)
//      println(cardCount)
//      println("".padTo('-',150))
//      println()

    }

    // If the last visible card was removed, time to show the hidden card underneath
    if (floorVisable(from).size == 0 && floorHidden(from).size > 0) {
      floorVisable(from) = floorHidden(from).take(1)
      floorHidden(from) = floorHidden(from).tail
    }

    // TODO: remove the column if it counts 13 to 1

    assert(floorVisable.size == 12)
    assert(floorHidden.size == 12)

  }

  /*
   * puts a card out of the deck onto each of the 10 columns
   */
  def playDeck() {
    if (deck.size == 0) {
      return
    }

    for (i <- 0 to 9) {
      floorVisable(i) = deck.head +: floorVisable(i)
      deck = deck.tail
    }

    history = (-1,-1,-1) +: history
  }


  override def toString = {
    getString(0, floorVisable.foldLeft(0)((max, c) => if (c.size > max) c.size else max ))
  }

  def getString(row: Int, max: Int): String = (row, max) match {
    case (row, max) if row >= max - 1 => {
      floorString(row)
    }
    case _  => {
      floorString(row) + "\n" + getString(row +1, max)
    }
  }


  def floorString(row: Int) = {
    if (floorVisable.size == 0)
      ""
    else
      floorVisable.foldLeft("")((string, column) => if (column.size > row) string + column.dropRight(row).last.toString.padTo(15, ' ') else string + "".padTo(15, ' '))
  }

  def floorHiddenString(row: Int) = {
    floorHidden.foldLeft("")((string, column) => if (column.size > row) string + column.dropRight(row).last.toString.padTo(15, ' ') else string + "".padTo(15, ' '))
  }

  def getHiidenString(row: Int, max: Int): String = (row, max) match {
    case (row, max) if row == max - 1 => {
      floorHiddenString(row)
    }
    case _  => {
      floorHiddenString(row) + "\n" + getHiidenString(row +1, max)
    }
  }

  def fullString = {
    "Deck: " + deck.mkString(", ") + "\n Hidden \n\n" + getHiidenString(0, floorHidden.foldLeft(0)((max, c) => if (c.size > max) c.size else max ))
  }

  def gameComplete = deck.size == 0 && floorVisable.flatten.size == 0 && floorHidden.flatten.size == 0

  override def equals(other: Any): Boolean =
    other match {

      case that: GameState => {
        // Logic for equlity here

        // Fast check if not equal
        if (this.deck.size != that.deck.size) {
          return false
        }

        // Deep check
        for (i <- 0 to 9) {
          if (this.floorVisable.size <= i || that.floorVisable.size <= i) {


            if (this.floorVisable.size == that.floorVisable.size) return {
              println(this)
              println(pad("M: " + this.history.take(5) + " score: " + this.score, 150, "-"))
              println()
              println()

              println(that)
              println(pad("M: " + this.history.take(5) + " score: " + this.score, 150, "-"))
              println()
              println()
              true
            } else
              false

          }
          else if (this.floorVisable(i).size != that.floorVisable(i).size) {
            return false
          } else {
            for (j <- 0 until this.floorVisable(i).size) {

              // If the card number and the suit do not match, the cards are different
              if (this.floorVisable(i)(j).n != that.floorVisable(i)(j).n && this.floorVisable(i)(j).getClass != that.floorVisable(i)(j).getClass) {
                return false
              }
            }
          }
        }

        // if everything turned out to match then we have a match
        return true
      }

      case _ => false
    }
  //
  //  def canEqual(other: Any): Boolean =
  //    other.isInstanceOf[GameState]
  //
  //  override def hashCode: Int = {
  //    this.floorVisable.toList.foldLeft(0)((hashCode, column) => column.foldLeft(0)((hc, card) => ((card.n + 41) + hc) * 41) * 41 )
  //  }


  def emptyFloorVisible: Boolean = floorVisable.foldLeft(true)((r, c) => if (c.size != 0) false else r)

  def canEqual(other: Any): Boolean = other.isInstanceOf[GameState]

//    override def equals(other: Any): Boolean = other match {
//      case that: GameState =>
//        (that canEqual this) &&
//          deck == that.deck &&
//          floorHidden == that.floorHidden &&
//          floorVisable == that.floorVisable
//      case _ => false
//    }

  override def hashCode(): Int = {
    val state = Seq(deck, floorHidden, floorVisable)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
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
