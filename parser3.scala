// Version 3
type Parser = String => Option[(String, String)]

def ok: Parser = (str: String) => Some(("", str))

def char(ch: Char): Parser = (str: String) => {
  if(! str.isEmpty && str.charAt(0) == ch) {
    Some((ch.toString, str.tail))
  } else None
}

val a = char('a')
val b = char('b')

def or(p1: Parser, p2: Parser): Parser = {
  (str: String) => p1(str).orElse(p2(str))
}

def and(p1: Parser, p2: Parser): Parser = {
  (str: String) => for {
    (f, rest1) <- p1(str)
    (s, rest2) <- p2(rest1)
  } yield (s"$f$s", rest2)
}


def option(p: Parser): Parser = {
  or(p, ok)
}

def rep(p: Parser): Parser = {
  (str: String) => {
    p(str) match {
      case None       => Some(("", str))
      case Some((f, rest1)) =>
        rep(p)(rest1).map { case (s, rest2) =>
          (s"$f$s", rest2)
        }
    }
  }
}

def rep1(p: Parser): Parser = {
  and(p, rep(p))
}

// Other common parser combinators

def oneOf(parsers: Seq[Parser]): Parser = ???

def noneOf(parsers: Seq[Parser]): Parser = ???

def not(parser: Parser): Parser = ???
