package scrainfuck

import scala.util.parsing.combinator.RegexParsers
import java.nio.charset.StandardCharsets

object BFParser {
  import java.io.{ ByteArrayInputStream, File, FileInputStream, Reader, InputStreamReader }
  import scala.util.{ Try }

  private def parser(desc: String, input: Reader) = {
    val parser = new BFParser()
    parser.parseAll(parser.program, input) match {
      case parser.Success(res, _) => util.Success(res)
      case parser.NoSuccess(msg, input) =>
        val (line, col) = (input.pos.line, input.pos.column)
        val lstr = input.pos.longString
        util.Failure(new RuntimeException(s"Error in '$desc' at line $line column $col.\n$msg\n$lstr"))
    }
  }
  
  def parseFile(file: File): Try[List[BFInstr]] =
    for {
      fis <- Try(new FileInputStream(file))
      isr <- Try(new InputStreamReader(fis))
      res <- parser(file.getAbsolutePath, isr)
      _ = isr.close()
      _ = fis.close()
    } yield res
    
  def parseString(s: String): Try[List[BFInstr]] =
    for {
      bts <- Try(s.getBytes(StandardCharsets.UTF_8))
      inp <- Try(new ByteArrayInputStream(bts))
      isr <- Try(new InputStreamReader(inp))
      res <- parser("string", isr)
      _ = isr.close()
      _ = inp.close()   
    } yield res
}

private class BFParser() extends RegexParsers {
  private val (nextSym, prevSym) = (">", "<")
  private val (incrSym, decrSym) = ("+", "-")
  private val (inptSym, ouptSym) = (",", ".")
  private val (loopBeg, loopEnd) = ("[", "]")
  private val allSyms = Seq(nextSym, prevSym, incrSym, decrSym, inptSym, ouptSym, loopBeg, loopEnd)
  
  private def comment = not(loopEnd) ~> ".".r
  private def program = rep(instr | comment) ^^ { _ collect { 
    case x: BFInstr => x
  }}

  private def instr = positioned(
    next | prev | incr | decr | inpt | oupt | loop)

  private def next: Parser[BFInstr] = nextSym ^^^ { Next }
  private def prev: Parser[BFInstr] = prevSym ^^^ { Prev }
  private def incr: Parser[BFInstr] = incrSym ^^^ { Incr }
  private def decr: Parser[BFInstr] = decrSym ^^^ { Decr }
  private def inpt: Parser[BFInstr] = inptSym ^^^ { Inpt }
  private def oupt: Parser[BFInstr] = ouptSym ^^^ { Oupt }
  private def loop: Parser[BFInstr] = loopBeg ~> program <~ loopEnd ^^ { Loop }
}