package oddco

/*
The algorithm
=============
16 consonant sounds:
  B X S K J T F H L M N P R 0 W Y (0 represents the "th" sound).

Exceptions:
  Initial kn-, gn-, pn, ac- or wr- → drop first letter
  Initial x- → change to "s"
  Initial wh- → change to "w"

Transformations:
  Vowels are kept only when they are the first letter.
  B → B unless at the end of a word after "m" as in "dumb"
  C → X (sh) if -cia- or -ch-
      S if -ci-, -ce- or -cy-
      K otherwise, including -sch-
  D → J if in -dge-, -dgy- or -dgi-
      T otherwise
  F → F
  G → silent in -gh- and not at end or before a vowel
      silent in -gn- or -gned- (also see dge etc. above)
      J if before i or e or y if not double gg
      K otherwise
  H → silent if after vowel and no vowel follows
      H otherwise
  J → J
  K → silent if after "c"
      K otherwise
  L → L
  M → M
  N → N
  P → F if in -ph-
      P otherwise
  Q → K
  R → R
  S → X (sh) if before "h" or in -sio- or -sia-
      S otherwise
  T → X (sh) if -tia- or -tio-
      0 (th) if before "h"
      silent if in -tch-
      T otherwise
  V → F
  W → silent if not followed by a vowel
      W if followed by a vowel
  X → KS
  Y → silent if not followed by a vowel
      Y if followed by a vowel
  Z → S
 */
object Metaphone  extends App {
  implicit class RichRegex(val sc: StringContext) extends AnyVal {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail: _*)
  }
  def translate(word: String): String = {
    val escaped = word.
      replaceFirst("^x", "s").
      replaceFirst("(?:^[kgp](n))|(?:^a(c))|(?:^w(r))|(?:^(w)h)", "$1$2$3$4")
    escaped.zipWithIndex.map {
      case (c, i) =>
        val (prefix, tail) = escaped.splitAt(i)
        (prefix, tail.head, tail.tail)
    }.flatMap {
      // Vowels are kept only when they are the first letter
      case ("", 'a', _) => "A"
      case ("", 'e', _) => "E"
      case ("", 'i', _) => "I"
      case ("", 'o', _) => "O"
      case ("", 'u', _) => "U"
      case (_, 'a', _) => ""
      case (_, 'e', _) => ""
      case (_, 'i', _) => ""
      case (_, 'o', _) => ""
      case (_, 'u', _) => ""
      // B → B unless at the end of a word after "m" as in "dumb"
      case (r".*m", 'b', "") => ""
      case (_, 'b', _) => "B"
      // C → K if -sch-
      case (r".*s", 'c', r"h.*") => "K"
      // C → X (sh) if -cia-
      case (_, 'c', r"ia.*") => "X"
      // C → X (sh) if -ch-
      case (_, 'c', r"h.*") => "X"
      // C → S if -ci-, -ce- or -cy-
      case (_, 'c', r"[iey].*") => "S"
      // C → K otherwise
      case (_, 'c', _) => "K"
      // D → J if in -dge-, -dgy or -dgi-
      case (_, 'd', r"g[eyi].*") => "J"
      // D → T otherwise
      case (_, 'd', _) => "T"
      // F → F
      case (_, 'f', _) => "F"
      // G → silent in -gh- and not at end or before vowel
      case (_, 'g', r"h[^aeiou]+.*") => ""
      // G → silent in -gn-
      case (_, 'g', r"n.*") => ""
      // G → silent in -gned-
      case (_, 'g', r"ned.*") => ""
      // G → silent (also see dge etc)
      case (r".*d", 'g', r"[eyi].*") => ""
      // G → K if second "g"
      case (r".*g", 'g', _) => "K"
      // G → J if before "i", "e" or "y"
      case (_, 'g', r"[iey].*") => "J"
      // G → K otherwise
      case (_, 'g', _) => "K"
      // H → silent if after vowel and no vowel follows
      case (r".*[aeiou]", 'h', r"[^aeiou].*") => ""
      // H → silent if after "p" (added because of sample data conformance [where "metaphone" --> "MTFN" is given],
      //                          and not according to specification [which produces "metaphone" --> "MTFHN" instead])
      case (r".*p", 'h', _) => ""
      // H → H otherwise
      case (_, 'h', _) => "H"
      // J → J
      case (_, 'j', _) => "J"
      // K → silent if after "c"
      case (r".*c", 'k', _) => ""
      // K → K otherwise
      case (_, 'k', _) => "K"
      // L → L
      case (_, 'l', _) => "L"
      // M → M
      case (_, 'm', _) => "M"
      // N → N
      case (_, 'n', _) => "N"
      // P → F if in -ph-
      case (_, 'p', r"h.*") => "F"
      // P → P
      case (_, 'p', _) => "P"
      // Q → K
      case (_, 'q', _) => "K"
      // R → R
      case (_, 'r', _) => "R"
      // S → X (sh) if before "h"
      case (_, 's', r"h.*") => "X"
      // S → X (sh) if in -sio- or -sia
      case (_, 's', r"i[oa].*") => "X"
      // S → S otherwise
      case (_, 's', _) => "S"
      // T → X (sh) if -tia- or -tio
      case (_, 't', r"i[ao].*") => "X"
      // T → 0 (th) if before "h"
      case (_, 't', r"h.*") => "0"
      // T → silent if in -tch-
      case (_, 't', r"ch.*") => ""
      // T → T otherwise
      case (_, 't', _) => "T"
      // V → F
      case (_, 'v', _) => "F"
      // W → W if followed by vowel
      case (_, 'w', r"[aeiou].*") => "W"
      // W → silent otherwise
      case (_, 'w', _) => ""
      // X → KS
      case (_, 'x', _) => "KS"
      // Y → Y if followed by vowel
      case (_, 'y', r"[aeiou].*") => "Y"
      // Y → silent otherwise
      case (_, 'y', _) => ""
      // Z → S
      case (_, 'z', _) => "S"
      // Leave anything else in place for debugging
      case (_, c, _) => c.toString
    }.mkString
  }
  if (args.isEmpty) println("java -jar metaphone.jar words to be translated to metaphone")
  else args.foreach(w => println(translate(w)))
}
