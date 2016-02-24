
import scala.io.StdIn
import scrainfuck._

object BFEval {

  private def zeroise(band: Map[Int, Char], ptr: Int): Char =
    band.get(ptr).getOrElse(0.toChar)

  def eval(instrs: List[BFInstr]): (Map[Int, Char], Int) =
    eval(instrs.toVector, 0, Map(), 0)

  def eval(instrs: Vector[BFInstr], ip: Int, band: Map[Int, Char], cell: Int): (Map[Int, Char], Int) =
    if (ip < instrs.size) instrs(ip) match {
      case Next => eval(instrs, ip + 1, band, cell + 1)
      case Prev => eval(instrs, ip + 1, band, cell - 1)
      case Incr =>
        val nband = band + (cell -> (zeroise(band, cell).toInt + 1).toChar)
        eval(instrs, ip + 1, nband, cell)
      case Decr =>
        val nband = band + (cell -> (zeroise(band, cell).toInt - 1).toChar)
        eval(instrs, ip + 1, nband, cell)
      case Inpt =>
        val c = StdIn.readChar()
        val nband = band + (cell -> c)
        eval(instrs, ip + 1, nband, cell)
      case Oupt =>
        print(zeroise(band, cell))
        eval(instrs, ip + 1, band, cell)
      case Loop(ns) =>
        var bnd = band
        var cll = cell
        while (zeroise(bnd, cll) != 0.toChar) {
          val (bres, cres) = eval(ns.toVector, 0, bnd, cll)
          bnd = bres
          cll = cres
        }
        eval(instrs, ip + 1, bnd, cll)
    }
    else band -> cell

}