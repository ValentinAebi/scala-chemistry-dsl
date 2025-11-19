package math

import scala.annotation.tailrec

@tailrec
def gcd(a: Int, b: Int): Int = {
  if a < 0 then gcd(-a, b)
  else if b < 0 then gcd(a, -b)
  else if a < b then gcd(b, a)
  else if b == 0 then a
  else gcd(b, a % b)
}

def gcd(numbers: Seq[Int]): Int = numbers.reduce(gcd)

def lcm(a: Int, b: Int): Int = Math.abs(a * b / gcd(a, b))

def lcm(numbers: Seq[Int]): Int = numbers.reduce(lcm)
