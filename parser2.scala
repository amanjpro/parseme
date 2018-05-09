// Version 2
type Parser = String => Option[String]

def ok: Parser = (str: String) => Some(str)

def char(ch: Char): Parser = (str: String) => {
  if(! str.isEmpty && str.charAt(0) == ch) {
    Some(str.tail)
  } else None
}

// Usage examples
val a = char('a')
val b = char('b')
