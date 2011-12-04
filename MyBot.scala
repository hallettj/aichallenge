import scala.math.{abs, min, max}

object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {

  type Move = Set[Order]
  val directions = List(North, East, South, West).par
  var game: Game = null
  var lastMove: Move = Set.empty
  var inertia: Map[Tile, CardinalPoint] = Map.empty

  val lostAntCost = 1000d
  val foodAttraction = 300d
  val waterRepulsion = 0.003d
  val inertiaScore = 6d
  val flockingScore = 3d

  val antsPerCluster = 4
  val clusterMargin = 10

  def ordersFrom(game: Game): Move = {
    this.game = game

    val clusters = findClusters(game)
    val emptyMove: Move = Set.empty
    val move = (emptyMove /: clusters) { (move, cluster) =>
      move ++ bestMove(cluster, emptyMove, cluster.myAnts.values.toList)
    }

    lastMove = move
    (for (order <- move) yield {
      (game.tile(order.point).of(order.tile), order.point)
    }).toMap

    move
  }

  def findClusters(game: Game): Set[Board] = {
    val params = game.parameters
    findClusters(game.board, Tile(0, 0), Tile(params.rows, params.columns), 'horizontal)
  }

  def findClusters(board: Board, upperLeft: Tile, lowerRight: Tile, split: Symbol): Set[Board] = {
    val nextSplit = if (split == 'horizontal) 'vertical else 'horizontal

    if (board.myAnts.isEmpty) {
      Set.empty
    } else if (board.myAnts.size <= antsPerCluster) {
      Set(board)
    } else {
      val antCount = board.myAnts.size

      val ants = board.myAnts.toList.sortBy { (antPosition) =>
        val (tile, ant) = antPosition
        if (split == 'horizontal) tile.row else tile.column
      }

      val (antsA, antsB) = ants.splitAt(antCount / 2)

      val (upperLeftA, lowerRightA) = boundsFor(antsA.map(_._2))
      val (upperLeftB, lowerRightB) = boundsFor(antsB.map(_._2))

      val boardA = Board(
        antsA.toMap,
        select(board.enemyAnts, upperLeftA, lowerRightA),
        select(board.water, upperLeftA, lowerRightA),
        select(board.food, upperLeftA, lowerRightA),
        select(board.corpses, upperLeftA, lowerRightA),
        select(board.myHills, upperLeftA, lowerRightA),
        select(board.enemyHills, upperLeftA, lowerRightA)
      )

      val boardB = Board(
        antsB.toMap,
        select(board.enemyAnts, upperLeftB, lowerRightB),
        select(board.water, upperLeftB, lowerRightB),
        select(board.food, upperLeftB, lowerRightB),
        select(board.corpses, upperLeftB, lowerRightB),
        select(board.myHills, upperLeftB, lowerRightB),
        select(board.enemyHills, upperLeftB, lowerRightB)
      )

      findClusters(boardA, upperLeftA, lowerRightA, nextSplit) ++
      findClusters(boardB, upperLeftB, lowerRightB, nextSplit)
    }
  }

  def boundsFor(items: List[Positionable]): (Tile, Tile) = {
    val first = items.head.tile
    val last = items.last.tile
    (Tile(first.column - clusterMargin, first.row - clusterMargin),
      Tile(last.column + clusterMargin, last.row + clusterMargin))
  }

  def select[A <: Positionable](items: Map[Tile, A], upperLeft: Tile, lowerRight: Tile): Map[Tile, A] = {
    items filter { (itemPosition) =>
      val (tile, item) = itemPosition
      tile.column >= upperLeft.column &&
        tile.row >= upperLeft.row &&
        tile.column <= lowerRight.column &&
        tile.row <= lowerRight.row
    }
  }

  def bestMove(board: Board, partialMove: Move, pendingAnts: List[MyAnt]): Move = pendingAnts match {
    case ant :: remainingAnts => {
      val possibleMoves = for {
        direction <- directions
        possibleMove = partialMove + Order(ant.tile, direction)
      } yield {
        val completeMove = bestMove(board, possibleMove, remainingAnts)
        val (score, message) = scoreOf(board, completeMove)
        ScoredMove(completeMove, score, message)
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

  def scoreOf(board: Board, move: Move): (Double, String) = {
    val positions = positionsAfter(board, move)

    val casualties = lostAntCosts(board, move, positions)
    val food = foodDrive(board, move)
    val obstacles = obstacleAvoidance(board, move)
    val explore = exploration(board, move, positions)
    val flock = flocking(board, move)

    val message = "casualties: "+ casualties +", food: "+ food +", obstacles: "+ obstacles +", explore: "+ explore +", flock: "+ flock

    (0d + casualties + food + obstacles + explore + flock, message)
  }

  def lostAntCosts(board: Board, move: Move, positions: Map[Tile, Iterable[Positionable]]): Double = {
    (0d /: positions) { (score, position) =>
      val (tile, ants) = position

      if (ants.size > 1)
        score - lostAntCost
      else if (board.water.contains(tile))
        score - lostAntCost
      else if (board.myHills.contains(tile))
        score - lostAntCost
      else
        score
    }
  }

  def foodDrive(board: Board, move: Move): Double = {
    val foodBonuses = for {
      (foodTile, food) <- board.food
    } yield {
      ((move filter { (order: Order) => movesToward(foodTile, order) }).toList sortBy { order =>
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

  def obstacleAvoidance(board: Board, move: Move): Double = {
    (for {
      order <- move
      (waterTile, water) <- board.water
      if movesToward(waterTile, order)
    } yield {
      val dist = distance(waterTile, order.tile)
      if (dist > 0)
        0d - (waterRepulsion / (dist * dist))
      else
        0d - waterRepulsion
    }).sum
  }

  def exploration(board: Board, move: Move, positions: Map[Tile, Iterable[Positionable]]): Double = {
    (0d /: positions) { (score, position) =>
      val (tile, ants) = position
      val points = for {
        lastPoint <- inertia.get(tile)
        order <- move find { _.tile == tile }
        thisPoint = order.point
      } yield {
        if (lastPoint == thisPoint)
          inertiaScore
        else
          0d
      }
      score + points.getOrElse(0d)
    }
  }

  def flocking(board: Board, move: Move): Double = {
    val antCount = board.myAnts.size
    val centerCol = (board.myAnts.values map { ant => ant.tile.column }).sum / antCount
    val centerRow = (board.myAnts.values map { ant => ant.tile.row }).sum / antCount
    val epicenter = Tile(centerCol, centerRow)

    (for {
        order <- move
        if movesToward(epicenter, order)
    } yield flockingScore).sum
  }

  def positionsAfter(board: Board, move: Move): Map[Tile, Iterable[Positionable]] = {
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

  def movesToward(goal: Tile, order: Order): Boolean = order.point match {
    case North => goal.row < order.tile.row
    case South => goal.row > order.tile.row
    case East => goal.column > order.tile.column
    case West => goal.column < order.tile.column
  }

  case class ScoredMove(val move: Move, val score: Double, val message: String)
  object ScoredMove {
    val empty: ScoredMove = new ScoredMove(Set.empty, -99999, "")
  }
}
