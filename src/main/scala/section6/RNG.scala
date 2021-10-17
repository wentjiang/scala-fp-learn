package com.wentjiang
package section6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  val intRand: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  //6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    val value = if (i < 0) -(i + 1) else i
    (value, r)
  }

  //6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = rng.nextInt
    (Math.abs(i - 1).toDouble / Integer.MAX_VALUE, r)
  }

  //6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  //6.3
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt
    ((d, i), r2)
  }

  //6.4
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, r) = s(rng)
      (f(a), r)
    }
  }

  //6.5
  def doubleViaMap(rng: RNG): Rand[Double] = {
    map[Int, Double](rng => rng.nextInt)(i => {
      Math.abs(i - 1).toDouble / Integer.MAX_VALUE
    })
  }

  //6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
    map2(ra, rb)((a, b) => (a, b))
  }

  //6.7 *
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
  }

  //6.8
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }


  /**
   *
   * @param f
   * @param g
   * @tparam A
   * @tparam B
   * @return
   */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng: RNG => {
      val (a, r) = f(rng)
      g(a)(r)
    }
  }

  //6.9
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s) { a => unit(f(a)) }
  }

  //6.9
  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra) { a => {
      flatMap(rb) { b => {
        unit(f(a, b))
      }
      }
    }
    }
  }

  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))



}
