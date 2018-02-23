// Version 4
type Parser[T] = String => Option[(T, String)]

def char(ch: Char): Parser[String] = (str: String) => {
  if(! str.isEmpty && str.charAt(0) == ch) {
    Some((ch.toString, str.tail))
  } else None
}

val a = char('a')
val b = char('b')

def or[T](p1: Parser[T], p2: Parser[T]): Parser[T] = {
  (str: String) => p1(str).orElse(p2(str))
}

def and[T](p1: Parser[T], p2: Parser[T]): Parser[(T, T)] = {
  (str: String) => for {
    (f, rest1) <- p1(str)
    (s, rest2) <- p2(rest1)
  } yield ((f, s), rest2)
}


def option[T](p: Parser[T]): Parser[Option[T]] = {
  (str: String) => p(str) match {
    case Some((r, rest)) => Some((Some(r), rest))
    case None            => Some((None, str))
  }
}

def rep[T](p: Parser[T]): Parser[Seq[T]] = {
  (str: String) => {
    p(str) match {
      case None       => Some((Seq.empty[T], str))
      case Some((f, rest1)) =>
        rep(p)(rest1).map { case (s, rest2) =>
          (f +: s, rest2)
        }
    }
  }
}

def rep1[T](p: Parser[T]): Parser[Seq[T]] = {
  (str: String) => {
    for {
      (f, rest1) <- p(str)
      (s, rest2) <- rep(p)(rest1)

    } yield (f +: s, rest2)
  }
}


// Other common parser combinators

def oneOf[T](parsers: Seq[Parser[T]]): Parser[T] = ???

def noneOf[T](parsers: Seq[Parser[T]]): Parser[T] = ???

def not[T](parser: Parser[T]): Parser[T] = ???
