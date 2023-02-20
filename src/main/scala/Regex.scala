import scala.collection.mutable

object Regex {
  /*
    This function should:
    -> Classify input as either character(or string) or operator
    -> Convert special inputs like [0-9] to their correct form
    -> Convert escaped characters
  */
  def preprocess(s:List[Char]): List[Either[Char,Char]] = {
    ???
  }

  def my_preprocess(s: String): String = {
    var ok = 0
    var new_s = ""

    val s1 = s.replace("eps", "@")
    val s2 = s1.replace("[0-9]","(0|1|2|3|4|5|6|7|8|9)")
    val s3 = s2.replace("[a-z]","(a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z)")
    val s4 = s3.replace("[A-Z]","(A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z)")
    for (i <- s4) {
      if (i.isLetter || i.equals('(') || i.isDigit) { //|| i.equals('*') || i.equals('(')
        if (ok == 1) {
          new_s = new_s + "&" + i
        }
        else
          new_s = new_s + i
        if (i.isLetter)
          ok = 1
        else
          ok = 0
      }
      else if(i.equals('*') || i.equals(')')){
        new_s = new_s + i
        ok = 1
      }
      else {
        new_s = new_s + i
        ok = 0
      }
    }
    new_s
  }

  def getPriority(C: Char): Int = {
    if ( C == '|')  1
    else if (C == '&')  2
    else if(C == '*')  3
    else 0
  }

  def infixToPostfix(infix1: String): String = {
    val infix = '(' + infix1 + ')'

    val char_stack = new mutable.Stack[Char]()
    var output = ""
    for (i <- infix) {
      if (i.isLetter || i.isDigit || i.equals('@'))
        output += i + " "
      else if (i == '(')
        char_stack.push('(')
      else if (i == ')')
      {
        while ( char_stack.top != '(')
        {
          output += char_stack.pop() + " "
        }
        char_stack.pop
      }
      else { // Operator found
        while (getPriority(i) <= getPriority(char_stack.top))
        {
          output += char_stack.pop() + " "
        }
        char_stack.push(i)
      }
    }
    while ( char_stack.nonEmpty)
      output += char_stack.pop + " "
    output
  }

  def infixToPrefix(infix: String): String = {

    // Reverse infix
    val infix1 = infix.reverse

    var l1: List[Int] = List()
    var l2: List[Int] = List()

    var cnt = 0
    for(i <- infix1)
    {
      if(i.equals('('))
        l1 = cnt :: l1
      else if (i.equals(')'))
        l2 = cnt :: l2
      cnt+=1
    }

    val sb1 = new mutable.StringBuilder(infix1)
    for (i <- l1) {
      sb1(i) = ')'
    }

    val sb2 = new mutable.StringBuilder(sb1.toString())
    for (i <- l2) {
      sb2(i) = '('
    }

    val prefix = infixToPostfix(sb2.toString())

    prefix.reverse
  }


  // This function should construct a prenex expression out of a normal one.
  def toPrenex(str: String): String = {
    val res = infixToPrefix(my_preprocess(str))
    var res2 = ""
    for (i <- res)
    {
      if(i.equals('&'))
      {
        res2 += "CONCAT"
      }
      else if(i.equals('|'))
      {
        res2 += "UNION"
      }
      else if(i.equals('*'))
      {
        res2 += "STAR"
      }
      else if(i.equals('@'))
      {
        res2 += "eps"
      }
      else
      {
        res2 += i
      }
    }
    res2.substring(1)
  }
}
