

import scala.util.{ Failure, Success }
import scrainfuck._
import scala.io.StdIn
import java.io.File

object Brainfuck {
  
  def main(args: Array[String]): Unit =
    if (args.length != 1) {
      println("please specify a file!")
      sys.exit(1)
    } else BFParser.parseFile(new File(args.head)) match {
      case Success(r) => run(r)
      case Failure(e) => println(e.getMessage)
    }
  
  private def zeroise(band: Map[Int, Char], ptr: Int): Char =
    band.get(ptr).getOrElse(0.toChar)
  
  private def run(instrs: List[BFInstr]): (Map[Int, Char], Int) =
    run(instrs.toVector, 0, Map(), 0)
    
  private def run(instrs: Vector[BFInstr], ip: Int, band: Map[Int, Char], cell: Int): (Map[Int, Char], Int) =
    if (ip < instrs.size) instrs(ip) match {
      case Next => run(instrs, ip + 1, band, cell + 1)
      case Prev => run(instrs, ip + 1, band, cell - 1)
      case Incr =>
        val nband = band + (cell -> (zeroise(band, cell).toInt + 1).toChar)
        run(instrs, ip + 1, nband, cell)
      case Decr => 
        val nband = band + (cell -> (zeroise(band, cell).toInt - 1).toChar)
        run(instrs, ip + 1, nband, cell)
      case Inpt =>
        val c = StdIn.readChar()
        val nband = band + (cell -> c)
        run(instrs, ip + 1, nband, cell)
      case Oupt =>
        print(zeroise(band, cell))
        run(instrs, ip + 1, band, cell)
      case Loop(ns) =>
        var bnd = band
        var cll = cell
        while (zeroise(bnd, cll) != 0.toChar) {
          val (bres, cres) = run(ns.toVector, 0, bnd, cll)
          bnd = bres
          cll = cres
        }
        run(instrs, ip + 1, bnd, cll)
    } else band -> cell
  
}