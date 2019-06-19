class Rational(n: Int, d: Int) {
    require(d != 0)

    private val g = gcd(n.abs, d.abs)
    val numer = n / g
    val denom = d / g

    def this(n: Int) = this(n, 1)

    override def toString = numer + "/" + denom

    def +(that: Rational) = new Rational(numer * that.denom + that.numer * denom, denom * that.denom)
    def +(that: Int) = new Rational(number + that * denom, denom)

    def *(that: Rational) = new Rational(numer * that.numer, denom * that.denom)

    // TODO: missing other operators like `/` and `-`

    def gcd(a: Int, b: Int) : Int = if (b == 0) a else gcd(b, a % b)
}

implicit def intToRational(x: Int) = new Rational(x)
