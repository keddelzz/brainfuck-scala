package scrainfuck.transpiler

import scrainfuck.BFInstr
import java.io.File
import java.io.FileOutputStream
import scala.util.Try
import java.io.PrintWriter

trait Transpiler {
  private val _tabs = collection.mutable.Map.empty[Int, String]
  protected def tabs(depth: Int) = _tabs.getOrElseUpdate(depth, "  " * depth)
  
  final def transpile(xs: List[BFInstr], output: File): Try[Unit] =
    for {
      fos <- Try(new FileOutputStream(output))
      prw <- Try(new PrintWriter(fos))
      _ = transpile(xs, prw)
      _ = prw.flush()
      _ = prw.close()
      _ = fos.close()
    } yield ()

  final def runlength[A](xs: List[A]): List[(Int, A)] = xs match {
    case Nil => Nil
    case hd :: tl =>
      val (fst, rst) = xs span (_ == hd)
      (fst.size -> hd) :: runlength(rst)
  }
  
  def transpile(xs: List[BFInstr], output: PrintWriter): Unit
  val fileExtension: String
}