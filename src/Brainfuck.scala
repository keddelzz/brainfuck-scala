

import scala.io.{ StdIn, Source }
import scala.util.{ Failure, Success, Try }
import scrainfuck._
import scrainfuck.transpiler._
import java.io.File
import BFParser.{ parseFile, parseString }

object Brainfuck {
  
  private val languages = Map[String, Transpiler](
    "c" -> CLang,    
    "spl" -> SPLLang,
    "js" -> JSLang
  )
  
  private def print(x: Any) = {
    Predef.print(x)
    System.out.flush()
  }
  
  private def runOrPrintError(insrs: Try[List[BFInstr]]) =
    insrs match {
      case Success(r) => run(r)
      case Failure(e) => println(e.getMessage)
    }
  
  private def printHelp(): Unit = {
    println(":<n>x<s>\t\trepeat the string <s> <n> times")
		println(":clear, :c\t\tclear input buffer")
    println(":exit, :quit, :q\texit repl")
    println(":help\t\t\tshow help")
    println(":run, :r\t\trun buffered program and clear input buffer")
  }
  
  private val repeat = ":([0-9]+)x(.*)".r

  private def runRepl() = {
    val bldr = new StringBuilder()
    printHelp()
    println()
    print("> ")
    for (line <- Source.stdin.getLines()) {
      line match {
        case repeat(n, str)           => for (_ <- 0 until n.toInt) bldr ++= str
        case ":help"                  => printHelp()
        case ":exit" | ":quit" | ":q" => sys.exit(0)
        case ":clear" | ":c"          => bldr.clear()
        case ":run" | ":r" =>
          runOrPrintError(parseString(bldr.toString))
          bldr.clear()
          println()
        case s => bldr ++= s
      }
      print("> ")
    }
  }
  
  def main(args: Array[String]): Unit =
    if (args.isEmpty) runRepl()
    else if (args.length == 1) 
      runOrPrintError(parseFile(new File(args.head)))
    else if ((args.length == 2 || args.length == 3) && args(1) == "-to") {
      if (args.length == 2) {
        print("possible output languages: ")
        println(languages.keys.toList.sorted.mkString(", "))
      } else {
        val transpiler = languages.get(args(2)).getOrElse {
          println("unknown output language '${args(2)}'!")
          print("possible output languages: ")
          println(languages.keys.mkString(", "))
          sys.exit(1)
        }
        def stripExtension(s: String) =
          (s indexOf ".") match {
            case i if i < 0 => s
            case i          => s.substring(0, i)
          }
        val input = new File(args.head)
        val output = new File(input.getParentFile, 
          stripExtension(input.getName) + "." + transpiler.fileExtension)
        parseFile(input) match {
          case Success(r) => 
            transpiler.transpile(r, output) match {
              case Success(_) =>
              case Failure(t) => println(t.getMessage) ; sys.exit(1)
            }
          case Failure(e) => println(e.getMessage) ; sys.exit(1)
        }
      }
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