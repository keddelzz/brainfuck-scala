package scrainfuck

import scala.util.parsing.input.Positional

sealed trait BFInstr extends Positional
case object Next extends BFInstr
case object Prev extends BFInstr
case object Incr extends BFInstr
case object Decr extends BFInstr
case object Inpt extends BFInstr
case object Oupt extends BFInstr
case class Loop(instrs: List[BFInstr]) extends BFInstr