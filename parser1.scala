// Version 1
type Parser = String => Boolean

def ok: Parser = (str: String) => true

def char(ch: Char): Parser = (str: String) => {
  ! str.isEmpty && str.charAt(0) == ch
}
