package scrainfuck.transpiler

import scrainfuck._
import java.io.PrintWriter

object JSLang extends Transpiler {
  private var braceDepth = 0
  val fileExtension = "html"
  
  def transpile(xs: List[BFInstr], pw: PrintWriter): Unit = {
    pw.print("""<!DOCTYPE html><html><head><title>brainfuck javascript</title><meta charset="utf-8"></head><body onload="start()">""")
    pw.print("""<style>textarea,input{border:0;background-color:black;color:white;font-family:'courier';font-size:9pt;position:absolute;top:50%;left:50%;outline:0}textarea{resize:none}input{}</style>""")
    pw.println("""<textarea cols="140" rows="50" disabled="disabled"></textarea><input type="text" placeholder="> " disabled="disabled"/>""")
    
    pw.println("""<script>var text=document.querySelector('textarea');var input=document.querySelector('input[type=text]');window.buffer="";""")
    pw.println("""function putchar(char){if(char.length>0 && char.charAt(0) !== '\r'){text.value+=char.charAt(0);}}""")
    pw.println("""function readchar(callback){input.removeAttribute("disabled");interval=setInterval(function(){if(window.buffer.length>0){var char=window.buffer.charAt(0);window.buffer=window.buffer.substr(1);input.setAttribute("disabled","disabled");clearInterval(interval);callback(char);}},10);}""")
    pw.println("""function inputHandler(e){if(e.keyCode===13){window.buffer+=input.value;input.value="";}} input.addEventListener("keydown",inputHandler,false);function resize(width,height){text.style.marginTop=(text.clientHeight+input.clientHeight)/-2+"px";input.style.top=text.offsetTop+text.clientHeight+"px";input.style.width=text.clientWidth+"px";text.style.marginLeft=-text.clientWidth/2+"px";input.style.marginLeft=text.style.marginLeft;} resize(window.innerWidth,window.innerHeight);window.onresize=function(){resize(window.innerWidth,window.innerHeight);};function start(){""")
    
    pw.println("""var band = new Array(1 << 20);""")
    pw.println("""var p = 0;""")
    pw.println("""for (var i = 0; i < band.length; i++) band[i] = 0;""")
    braceDepth = 0
    
    val ys = runlength(xs)
    for ((cnt, ele) <- ys) {
      transpile(ele, 1, pw, cnt)
    }
    (0 until braceDepth).foreach(_ => pw.print("}"))
    pw.println("""}</script> </body></html>""")
  }
    
  private def transpile(instr: BFInstr, depth: Int, pw: PrintWriter, cnt: Int): Unit = instr match {
    case Incr => { pw.print(tabs(depth)) ; pw.println(s"band[p] += $cnt;") }
    case Decr => { pw.print(tabs(depth)) ; pw.println(s"band[p] -= $cnt;") }
    case Next => { pw.print(tabs(depth)) ; pw.println(s"p += $cnt;") }
    case Prev => { pw.print(tabs(depth)) ; pw.println(s"p -= $cnt;") }
    case Inpt => { (0 until cnt).foreach { _ => pw.print(tabs(depth)) ; pw.println("readchar(function(c) {"); braceDepth += 1 } }
    case Oupt => { (0 until cnt).foreach { _ => pw.print(tabs(depth)) ; pw.println("putchar(String.fromCharCode(band[p]));") } }
    case Loop(ins) => {
      pw.print(tabs(depth)) ; pw.println("while (band[p] !== 0) {")
      val ys = runlength(ins)
      for ((cnt, ele) <- ys) {
        transpile(ele, depth + 1, pw, cnt)
      }
      pw.print(tabs(depth)) ; pw.println("}")
    }
  }
  
}