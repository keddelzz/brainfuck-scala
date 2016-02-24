package scrainfuck

import java.io.{ ByteArrayInputStream, File, FileInputStream, Reader, InputStreamReader }
import java.nio.charset.StandardCharsets

import scala.util.{ Try }
import scala.util.parsing.combinator.RegexParsers

object BFParser {

  private def handleResult[T](desc: String, parser: BFParser)(result: parser.ParseResult[T]): Try[T] =
    result match {
      case parser.Success(res, _) => util.Success(res)
      case parser.NoSuccess(msg, input) =>
        val (line, col) = (input.pos.line, input.pos.column)
        val lstr = input.pos.longString
        util.Failure(new RuntimeException(s"Error in '$desc' at line $line column $col.\n$msg\n$lstr"))
    }

  private def parse(desc: String, input: Either[Reader, CharSequence]) = {
    val parser = new BFParser()
    val result = input.fold(parser.parseAll(parser.program, _), parser.parseAll(parser.program, _))
    handleResult(desc, parser)(result)
  }

  def parseFile(file: File): Try[List[BFInstr]] =
    for {
      fis <- Try(new FileInputStream(file))
      isr <- Try(new InputStreamReader(fis))
      res <- parse(file.getAbsolutePath, Left(isr))
      _ = isr.close()
      _ = fis.close()
    } yield res

  def parseString(s: String): Try[List[BFInstr]] =
    parse("string", Right(s))
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