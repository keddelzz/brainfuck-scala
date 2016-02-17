package scrainfuck.transpiler

import scrainfuck._
import scala.util.{ Failure, Success }
import java.io.FileOutputStream
import java.io.PrintWriter

object CLang extends Transpiler {
  
  val fileExtension: String = "c"
  
  def transpile(xs: List[BFInstr], pw: PrintWriter): Unit = {
    pw.println("""
      |#include <stdio.h>
      |#include <stdlib.h>
      |
      |int main(int argc, char* argv[]) {
      |  char band[1 << 20];
      |  char* p = (char*) band;
      |""".stripMargin)
    xs.foreach(transpile(_, 1, pw))
    pw.println("""
      |  return EXIT_SUCCESS;
      |}
      |""".stripMargin)
  }

  private def transpile(instr: BFInstr, depth: Int, pw: PrintWriter): Unit = instr match {
    case Incr => { pw.print(tabs(depth)) ; pw.println("++*p;") } 
    case Decr => { pw.print(tabs(depth)) ; pw.println("--*p;") } 
    case Next => { pw.print(tabs(depth)) ; pw.println("p++;") }
    case Prev => { pw.print(tabs(depth)) ; pw.println("p--;") }
    case Inpt => { pw.print(tabs(depth)) ; pw.println("*p = (char) getchar();") }
    case Oupt => { pw.print(tabs(depth)) ; pw.println("putchar(*p);") }
    case Loop(ins) => 
      pw.print(tabs(depth)) ; pw.println("while (*p) {")
      ins.foreach(transpile(_, depth + 1, pw))
      pw.print(tabs(depth)) ; pw.println("}")
  }

}
