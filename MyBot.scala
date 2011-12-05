import scala.math.{abs, min, max}
import scala.util.Random

object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {

  type Move = Set[Order]
  val rand = new Random(System.currentTimeMillis)

  val directions = List(North, East, South, West)
  var game: Game = null
  var lastMove: Move = Set.empty
  var inertia: Map[Tile, CardinalPoint] = Map.empty

  val lostAntCost = 1000d
  val foodAttraction = 300d
  val waterRepulsion = 0.003d
  val inertiaScore = 12d
  val flockingScore = 3d
  val packingScore = 0.008d

  val antsPerCluster = 4
  val clusterMargin = 10

  def ordersFrom(game: Game): Move = {
    this.game = game

    val clusters = antClusters(game)
    val emptyMove: Move = Set.empty
    val move = (emptyMove /: clusters) { (move, cluster) =>
      move ++ bestMove(cluster, emptyMove, cluster.myAnts.values.toList)
    }

    lastMove = move
    inertia = (for (order <- move) yield {
      (game.tile(order.point).of(order.tile), order.point)
    }).toMap

    move
  }

  def antClusters(game: Game): Set[Board] = {
    val antCount = game.board.myAnts.size
    val ants = game.board.myAnts.values
    val numClusters = max(1, antCount / antsPerCluster)
    val centroids = ants take numClusters map { _.tile }
    val clusters = findClusters(ants, centroids, Set.empty[Set[MyAnt]])

    clusters map { cluster =>
      val (upperLeft, lowerRight) = boundsFor(cluster)
      val board = game.board
      Board(
        (cluster map { ant => (ant.tile -> ant) }).toMap,
        select(board.enemyAnts, upperLeft, lowerRight),
        select(board.water, upperLeft, lowerRight),
        select(board.food, upperLeft, lowerRight),
        select(board.corpses, upperLeft, lowerRight),
        select(board.myHills, upperLeft, lowerRight),
        select(board.enemyHills, upperLeft, lowerRight)
      )
    }
  }

  def findClusters[A <: Positionable](elements: Iterable[A], centroids: Iterable[Tile], clusters: Set[Set[A]]): Set[Set[A]] = {
    val newCentroids = if (clusters.isEmpty) centroids else clusters map { centroid(_) }
    val newClusters = ((elements groupBy { element =>
      centroids minBy { centroid => distance(element, centroid) }
    }).values map { _.toSet }).toSet

    if (newClusters == clusters)
      clusters
    else
      findClusters(elements, newCentroids, newClusters)
  }

  def boundsFor(items: Iterable[Positionable]): (Tile, Tile) = {
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
        direction <- rand.shuffle(directions).par
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
    val pack = packing(board, move, positions)

    val message = "casualties: "+ casualties +", food: "+ food +", obstacles: "+ obstacles +", explore: "+ explore +", flock: "+ flock +", pack: "+ pack

    (0d + casualties + food + obstacles + explore + flock + pack, message)
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
    val Order(leaderTile, leaderDirection) = leader(board, move)
    val goal = game.tile(leaderDirection).of(leaderTile)

    (for {
      order <- move
      if order.tile != leaderTile && movesToward(goal, order)
    } yield flockingScore).sum
  }

  def packing(board: Board, move: Move, positions: Map[Tile, Iterable[Positionable]]): Double = {
    val Order(leaderTile, leaderDirection) = leader(board, move)
    val goal = game.tile(leaderDirection).of(leaderTile)

    (for {
      (tile, ants) <- positions
      ant <- ants
    } yield {
      val dist = distance(goal, tile)
      -(packingScore * dist * dist)
    }).sum
  }

  def leader(board: Board, move: Move): Order = {
    val directionCounts = move groupBy { _.point } map { e => (e._1, e._2.size) }
    val prevailing = (directionCounts maxBy { _._2 })._1
    val leaderRank: Order => Int = prevailing match {
      case North => { order => order.tile.row }
      case South => { order => -order.tile.row }
      case East => { order => -order.tile.column }
      case West => { order => order.tile.column }
    }
    // val leaderTile = (move minBy { leaderRank(_) }).tile
    // game.tile(prevailing).of(leaderTile)
    move minBy { leaderRank(_) }
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

  def centroid(elements: Iterable[Positionable]): Tile = {
    val count = elements.size
    val centerCol = (elements map { _.column }).sum / count
    val centerRow = (elements map { _.row }).sum / count
    Tile(centerCol, centerRow)
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

  implicit def positionable2Tile(p: Positionable): Tile = p.tile
}
