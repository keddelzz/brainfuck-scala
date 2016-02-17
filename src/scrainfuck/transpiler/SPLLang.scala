package scrainfuck.transpiler

import scrainfuck._
import scala.util.{ Failure, Success }
import java.io.FileOutputStream
import java.io.PrintWriter

object SPLLang extends Transpiler {
  
  val fileExtension: String = "spl"
  
  def transpile(xs: List[BFInstr], pw: PrintWriter): Unit = {
    pw.println("""
      |proc main() {
      |  var band: array[1000000] of int;
      |  var p: int;
      |  p := 0;
      |  while (p < 1000000) {
      |    band[p] := 0;
      |    p := p + 1;
      |  }
      |  p := 0;
      |""".stripMargin)
    xs.foreach(transpile(_, 1, pw))
    pw.println("\n}")
  }
    
  private def transpile(instr: BFInstr, depth: Int, pw: PrintWriter): Unit = instr match {
    case Incr => { pw.print(tabs(depth)) ; pw.println("band[p] := band[p] + 1;") }
    case Decr => { pw.print(tabs(depth)) ; pw.println("band[p] := band[p] - 1;") }
    case Next => { pw.print(tabs(depth)) ; pw.println("p := p + 1;") }
    case Prev => { pw.print(tabs(depth)) ; pw.println("p := p - 1;") }
    case Inpt => { pw.print(tabs(depth)) ; pw.println("readc(band[p]);") }
    case Oupt => { pw.print(tabs(depth)) ; pw.println("printc(band[p]);") }
    case Loop(ins) => {
      pw.print(tabs(depth)) ; pw.println("while (band[p] # 0) {")
      ins.foreach(transpile(_, depth + 1, pw))
      pw.print(tabs(depth)) ; pw.println("}")
    }
  }
  
}