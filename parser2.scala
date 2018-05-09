// Version 2
type Parser = String => Option[String]

def ok: Parser = (str: String) => Some(str)
