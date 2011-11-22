object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {

  type Move = Set[Order]
  val directions = List(North, East, South, West)

  val lostAntCost = 100

  def ordersFrom(game: Game): Move = {
    bestMove(game, Set.empty, game.board.myAnts.values.toList)
  }

  def bestMove(game: Game, partialMove: Move, pendingAnts: List[MyAnt]): Move = pendingAnts match {
    case ant :: remainingAnts => {
      val possibleMoves = for {
        direction <- directions
        possibleMove = partialMove + Order(ant.tile, direction)
        // partialScore = partialScoreOf(game, possibleMove)
        // if partialScore > lowScoreThreshold
      } yield {
        val completeMove = bestMove(game, possibleMove, remainingAnts)
        val score = scoreOf(game, completeMove)
        ScoredMove(completeMove, score)
      }

      val bestScoredMove = (ScoredMove.empty /: possibleMoves) { (bestMove, possibleMove) =>
        if (possibleMove.score > bestMove.score)
          possibleMove
        else
          bestMove
      }

      bestScoredMove.move
    }
    case Nil => partialMove
  }

  def scoreOf(game: Game, move: Move): Int = {
    0 + lostAntCosts(game, move)
  }

  def lostAntCosts(game: Game, move: Move): Int = {
    val positions = positionsAfter(game, move)

    (0 /: positions) { (score, position) => 
      val (tile, ants) = position

      if (ants.size > 1)
        score - lostAntCost
      else if (game.board.water.contains(tile))
        score - lostAntCost
      else if (game.board.myHills.contains(tile))
        score - lostAntCost
      else
        score
    }
  }

  def positionsAfter(game: Game, move: Move): Map[Tile, Iterable[Positionable]] = {
    (Map.empty[Tile, List[Positionable]] /: move) { (positions, order) =>
      val antPos = order.tile
      val newPos = game.tile(order.point).of(order.tile)
      val existing = positions.getOrElse(newPos, List.empty[Positionable])
      positions.updated(newPos, MyAnt(newPos) :: existing)
    }
  }

  case class ScoredMove(val move: Move, val score: Int)
  object ScoredMove {
    val empty: ScoredMove = new ScoredMove(Set.empty, -99999)
  }

}
