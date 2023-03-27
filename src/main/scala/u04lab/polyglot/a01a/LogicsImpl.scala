package u04lab.polyglot.a01a
import Logics.*

import u04lab.code.List.*

import scala.util.Random
/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
class LogicsImpl(private val size: Int, private val boat: Int) extends Logics:
  private val hit: u04lab.code.List[(Int, Int)] = empty
  private var failures: Int = 0
  private val FAILURES: Int = 5
  private val boatRow: Int = Random.nextInt(size)
  private val boatLeftCol: Int = Random.nextInt(size - boat + 1)

  def hit(row: Int, col: Int): Result =
    if row == this.boatRow && col >= this.boatLeftCol && col < this.boatLeftCol + boat then
      { append(hit, cons((row, col), empty))
       if length(this.hit) == this.boat then Result.WON else Result.HIT}
    else {this.failures + 1
    if this.failures == FAILURES then Result.LOST else Result.MISS}

