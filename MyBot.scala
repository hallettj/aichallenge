import scala.math.abs

object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {

  type Move = Set[Order]
  val directions = List(North, East, South, West)

  val lostAntCost = 100d
  val foodAttraction = 6d
  val waterRepulsion = 0.003d

  val lowScoreThreshold = 0.2d

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

  def scoreOf(game: Game, move: Move): Double = {
    val casualties = lostAntCosts(game, move)
    val food = foodDrive(game, move)
    val obstacles = obstacleAvoidance(game, move)

    0d + casualties + food + obstacles
  }

  def lostAntCosts(game: Game, move: Move): Double = {
    val positions = positionsAfter(game, move)

    (0d /: positions) { (score, position) =>
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

  def foodDrive(game: Game, move: Move): Double = {
    val foodBonuses = for {
      (foodTile, food) <- game.board.food
    } yield {
      ((move filter { (order: Order) => movesToward(food, order) }).toList sortBy { order =>
        distance(foodTile, order.tile)
      } take 1 map { order =>
        val dist = distance(foodTile, order.tile)
        if (dist > 0)
          foodAttraction / (dist * dist)
        else
          foodAttraction
      }).sum
    }

    foodBonuses.sum
  }

  def obstacleAvoidance(game: Game, move: Move): Double = {
    (for {
      order <- move
      (waterTile, water) <- game.board.water
      if movesToward(water, order)
    } yield {
      val dist = distance(waterTile, order.tile)
      if (dist > 0)
        0d - (waterRepulsion / (dist * dist))
      else
        0d - waterRepulsion
    }).sum
  }

  def positionsAfter(game: Game, move: Move): Map[Tile, Iterable[Positionable]] = {
    (Map.empty[Tile, List[Positionable]] /: move) { (positions, order) =>
      val antPos = order.tile
      val newPos = game.tile(order.point).of(order.tile)
      val existing = positions.getOrElse(newPos, List.empty[Positionable])
      positions.updated(newPos, MyAnt(newPos) :: existing)
    }
  }

  def distance(a: Tile, b: Tile): Double = {
    val x = b.column - a.column
    val y = b.row - a.row
    abs(x) + abs(y)
  }

  def movesToward(goal: Positionable, order: Order): Boolean = order.point match {
    case North => goal.tile.row < order.tile.row
    case South => goal.tile.row > order.tile.row
    case East => goal.tile.column > order.tile.column
    case West => goal.tile.column < order.tile.column
  }

  case class ScoredMove(val move: Move, val score: Double)
  object ScoredMove {
    val empty: ScoredMove = new ScoredMove(Set.empty, -99999)
  }

}
