package hedgehog.arbitrary

import shapeless.poly.~>
import hedgehog._
import hedgehog.core._
import cats.implicits._

import shapeless._

trait Arbitrary[A] {
  def gen: Gen[A]
}

object Arbitrary extends ArbitraryLow {
  def apply[T](implicit ev: Lazy[Arbitrary[T]]): Arbitrary[T] = ev.value

  def fromGen[T](gs: => Gen[T]): Arbitrary[T] = new Arbitrary[T] { def gen = gs }

  val MAX_STRING_SIZE = 1024

  implicit val arbBigDecimal: Arbitrary[BigDecimal] = fromGen {
    Gen
      .double(Range.linearFracFrom(0, Double.MinValue, Double.MaxValue))
      .ensure {
        case Double.NegativeInfinity => false
        case Double.PositiveInfinity => false
        case _                       => true
      }
      .map(BigDecimal(_))
  }

  implicit val arbChar: Arbitrary[Char] = fromGen(Gen.char(Char.MinValue, Char.MaxValue))
  implicit val arbString: Arbitrary[String] = fromGen(
    arbChar.gen.list(Range.linear(0, MAX_STRING_SIZE)).map(_.mkString(""))
  )
  implicit val arbDouble: Arbitrary[Double]   = fromGen(Gen.double(Range.linearFracFrom(0, Double.MinValue, Double.MaxValue)))
  implicit val arbBoolean: Arbitrary[Boolean] = fromGen(Gen.boolean)
  implicit val arbByte: Arbitrary[Byte]       = fromGen(Gen.byte(Range.linear(Byte.MinValue, Byte.MaxValue)))
  implicit val arbShort: Arbitrary[Short]     = fromGen(Gen.short(Range.linearFrom(0, Short.MinValue, Short.MaxValue)))
  implicit val arbInt: Arbitrary[Int]         = fromGen(Gen.int(Range.linearFrom(0, Int.MinValue, Int.MaxValue)))
  implicit val arbLong: Arbitrary[Long]       = fromGen(Gen.long(Range.linearFrom(0, Long.MinValue, Long.MaxValue)))

  implicit def arbOption[V](implicit v: Lazy[Arbitrary[V]]): Arbitrary[Option[V]] =
    fromGen(v.value.gen.option)
  implicit def arbList[V](implicit v: Lazy[Arbitrary[V]]): Arbitrary[List[V]] =
    fromGen(v.value.gen.list(Range.linear(0, 100)))
  implicit def arbSet[V](implicit v: Lazy[Arbitrary[List[V]]]): Arbitrary[Set[V]] =
    fromGen(v.value.gen.map(_.toSet))
  implicit def arbMap[K, V](implicit k: Lazy[Arbitrary[Set[K]]], v: Lazy[Arbitrary[V]]): Arbitrary[Map[K, V]] =
    fromGen(
      k.value.gen.flatMap { keys =>
        v.value.gen.list(Range.constant(keys.size, keys.size)).map { values =>
          keys.toSeq.zip(values).toMap
        }
      }
    )

}

trait ArbitraryLow {

  type LList[A] = List[Lazy[A]]

  implicit val hnil: Arbitrary[HNil] = Arbitrary.fromGen[HNil](Gen.constant(HNil))

  implicit def hlist[A, T <: HList](implicit h: Cached[Arbitrary[A]], tl: Lazy[Arbitrary[T]]): Arbitrary[A :: T] =
    Arbitrary.fromGen {
      for {
        aGens  <- h.value.gen
        tlGens <- tl.value.gen
      } yield aGens :: tlGens
    }

  implicit def coproductEnd[A](implicit h: Lazy[Arbitrary[A]]): LList[Arbitrary[A :+: CNil]] =
    List(h.map(a => Arbitrary.fromGen(a.gen.map(Inl(_)))))

  implicit def coproduct[A, T <: Coproduct](implicit h: Lazy[Arbitrary[A]], tl: Lazy[LList[Arbitrary[T]]]): LList[Arbitrary[A :+: T]] =
    h.map(lh => Arbitrary.fromGen(lh.gen.map(Inl(_).asInstanceOf[A :+: T]))) :: tl.value.map(
      _.map(a => Arbitrary.fromGen(a.gen.map(Inr(_).asInstanceOf[A :+: T])))
    )

  implicit def genericProduct[A, L <: HList](implicit g: Generic.Aux[A, L], arb: Lazy[Arbitrary[L]]): Arbitrary[A] =
    Arbitrary.fromGen(arb.value.gen.map(g.from))

  def recoverDiscarded[A](from: Gen[A])(recover: => Gen[A]): Gen[A] =
    GenT { (size, seed) =>
      from.run(size, seed) match {
        case Tree((_, None), _) => recover.run(size, seed)
        case ret                => ret
      }
    }

  implicit def genericCoproduct[A, L <: Coproduct](implicit g: Generic.Aux[A, L], arb: Lazy[LList[Arbitrary[L]]]): Arbitrary[A] =
    Arbitrary.fromGen {
      Gen.sized {
        case Size(s) if s > 0 =>
          def pick(ls: List[Lazy[Arbitrary[L]]]): Gen[L] =
            Gen.int(Range.constant(0, ls.length - 1)).flatMap { i =>
              val (before, after) = ls.splitAt(i)
              after.headOption
                .map { a =>
                  recoverDiscarded(a.value.gen)(pick(before ++ after.tail))
                }
                .getOrElse(Gen.discard)
            }

          pick(arb.value).map(g.from).resize(Size(s - 1))
        case _ => Gen.discard
      }
    }
}
