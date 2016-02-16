package scrainfuck

import org.junit.Test
import org.junit.Assert._
import BFParser.{parseString, parseFile}

class ParserTest {
  
  @Test
  def onlySimpleProgram = {
    val expect = List(Incr, Incr, Incr)
    val actual = parseString("+++").get
    assertEquals(expect, actual)
  }
  
  @Test
  def onlyComment = {
    val expect = List()
    val actual = parseString("Hello World!").get
    assertEquals(expect, actual)
  }
  
  @Test
  def incrWithComment = {
    val expect = List(Incr)
    val actual = parseString("+Hello World!").get
    assertEquals(expect, actual)
  }
  
  @Test
  def emptyLoop = {
    val expect = List(Loop(List()))
    val actual = parseString("[]").get
    assertEquals(expect, actual)
  }
  
  @Test
  def helloWorld = {
    val expect = List(
      Loop(List(
        Inpt, Oupt, 
        Loop(List(Oupt)), 
        Inpt, Oupt, Oupt, Inpt, Inpt, Inpt, Incr, Inpt, Decr, Inpt, Prev, Next, Inpt, 
        Loop(List()), Oupt, Oupt
      )), 
      Incr, Incr, Incr, Incr, Incr, Incr, Incr, Incr, 
      Loop(List(
        Next, Incr, Incr, Incr, Incr, 
        Loop(List(
          Next, Incr, Incr, 
          Next, Incr, Incr, Incr, 
          Next, Incr, Incr, Incr, 
          Next, Incr, 
          Prev, Prev, Prev, Prev, Decr
        )), 
        Next, Incr, 
        Next, Incr, 
        Next, Decr, 
        Next, Next, Incr, 
        Loop(List(Prev)), 
        Prev, Decr
      )), 
      
      Next, Next, Oupt, 
      Next, Decr, Decr, Decr, Oupt, 
      Incr, Incr, Incr, Incr, Incr, Incr, Incr, Oupt, Oupt, Incr, Incr, Incr, Oupt, 
      Next, Next, Oupt, 
      Prev, Decr, Oupt, 
      Prev, Oupt, 
      Incr, Incr, Incr, Oupt, Decr, Decr, Decr, Decr, Decr, Decr, Oupt, Decr, Decr, Decr, Decr, Decr, Decr, Decr, Decr, Oupt, 
      Next, Next, Incr, Oupt, 
      Next, Incr, Incr, Oupt
    )
    val actual = parseString(HelloBrainFuck.helloWorld).get
    assertEquals(expect, actual)
  }
  
}