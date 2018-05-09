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


def or(p1: Parser, p2: Parser): Parser = {
  (str: String) => p1(str).orElse(p2(str))
}

def and(p1: Parser, p2: Parser): Parser = {
  (str: String) => p1(str).flatMap(rest => p2(rest))
}

def option(p: Parser): Parser = {
  or(p, ok)
}

def rep(p: Parser): Parser = {
  (str: String) => {
    p(str) match {
      case None       => Some(str)
      case Some(str2) => rep(p)(str2)
    }
  }
}

def rep1(p: Parser): Parser = {
  and(p, rep(p))
}
