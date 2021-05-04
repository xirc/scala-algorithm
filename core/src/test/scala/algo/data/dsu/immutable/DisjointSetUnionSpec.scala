package algo.data.dsu.immutable

import algo.testing.BaseSpec
import cats.Monoid
import cats.data.State
import cats.syntax.all._

final class DisjointSetUnionSpec extends BaseSpec {
  import DisjointSetUnion._

  def sequence[V, U: Monoid](
      iterable: Iterable[State[DisjointSetUnion[V], U]]
  ): State[DisjointSetUnion[V], U] = {
    iterable.foldLeft(State.empty[DisjointSetUnion[V], U])((cur, next) =>
      cur *> next
    )
  }

  "size" in {

    DisjointSetUnion.fill(1)(1).size shouldBe 1
    DisjointSetUnion.fill(10)(1).size shouldBe 10
    DisjointSetUnion.fill(100)(1).size shouldBe 100
    DisjointSetUnion.fill(1_000)(1).size shouldBe 1_000
    DisjointSetUnion.fill(10_000)(1).size shouldBe 10_000
    DisjointSetUnion.fill(100_000)(1).size shouldBe 100_000
    DisjointSetUnion.fill(1_000_000)(1).size shouldBe 1_000_000

  }

  "find" in {

    inside(DisjointSetUnion.fill(100)(1)) { dsu =>
      val checkValues = sequence(
        (0 until 100).map { index =>
          find[Int](index).map(value => value shouldBe 1).map(_ => ())
        }
      )
      checkValues.run(dsu).value
    }

    inside(DisjointSetUnion.tabulate(100)(identity)) { dsu =>
      val checkValues = sequence(
        (0 until 100).map { index =>
          find[Int](index).map(_ shouldBe index).map(_ => ())
        }
      )
      checkValues.run(dsu).value
    }

    inside(DisjointSetUnion.fill(10)(1)) { dsu =>
      a[IndexOutOfBoundsException] shouldBe thrownBy {
        dsu.find(-1)
      }
      a[IndexOutOfBoundsException] shouldBe thrownBy {
        dsu.find(dsu.size)
      }
    }

  }

  "isSame" in {

    val dsu = DisjointSetUnion.fill(10)(1)

    val checkSame = sequence(
      for {
        u <- 0 until dsu.size
        v <- 0 until dsu.size
      } yield {
        val expectedSame = u == v
        isSame[Int](u, v).map(_ shouldBe expectedSame).map(_ => ())
      }
    )
    checkSame.run(dsu).value

    a[IndexOutOfBoundsException] shouldBe thrownBy {
      dsu.isSame(-1, 1)
    }

    a[IndexOutOfBoundsException] shouldBe thrownBy {
      dsu.isSame(dsu.size, 1)
    }

    a[IndexOutOfBoundsException] shouldBe thrownBy {
      dsu.isSame(1, -1)
    }

    a[IndexOutOfBoundsException] shouldBe thrownBy {
      dsu.isSame(1, dsu.size)
    }

  }

  "unite" in {

    val dsu = DisjointSetUnion.fill(10)(1)

    val checkUnite = for {
      _ <- unite[Int](1, 3)
      same <- isSame(1, 3)
      uValue <- find(1)
      vValue <- find(3)
    } yield {
      same shouldBe true
      uValue shouldBe vValue
    }
    checkUnite.run(dsu).value

    val checkUniteSameGroup = for {
      _ <- unite[Int](2, 4)
      s1 <- State.get
      // Unite same group
      _ <- unite[Int](4, 2)
      s2 <- State.get
    } yield {
      s1 shouldBe s2
    }
    checkUniteSameGroup.run(dsu).value

    a[IndexOutOfBoundsException] shouldBe thrownBy {
      dsu.united(-1, 3)
    }

    a[IndexOutOfBoundsException] shouldBe thrownBy {
      dsu.united(dsu.size, 3)
    }

    a[IndexOutOfBoundsException] shouldBe thrownBy {
      dsu.united(-1, -1)
    }

    a[IndexOutOfBoundsException] shouldBe thrownBy {
      dsu.united(1, dsu.size)
    }

  }

  "factory:from" in {

    val xs = Vector(1, 2, 3, 4, 5)
    val dsu = DisjointSetUnion.from(xs.iterator)
    val checkFrom = for {
      _ <- DisjointSetUnion.size.map(_ shouldBe 5)
      _ <- sequence(for (v <- 0 until dsu.size) yield {
        find[Int](v).map(_ shouldBe (v + 1)).map(_ => ())
      })
    } yield ()
    checkFrom.run(dsu).value

  }

  "factory:apply" in {

    val dsu = DisjointSetUnion(1, 2, 3, 4, 5)

    val checkApply = for {
      _ <- DisjointSetUnion.size.map(_ shouldBe 5)
      _ <- sequence(for (v <- 0 until dsu.size) yield {
        find[Int](v).map(_ shouldBe (v + 1)).map(_ => ())
      })
    } yield ()
    checkApply.run(dsu).value

  }

  "factory:iterate" in {

    val dsu = DisjointSetUnion.iterate(1, 10)(_ * 2)

    val checkIterate = for {
      _ <- DisjointSetUnion.size.map(_ shouldBe 10)
      _ <- sequence(for (v <- 0 until dsu.size) yield {
        find[Int](v).map(_ shouldBe (1 << v)).map(_ => ())
      })
    } yield ()
    checkIterate.run(dsu).value

  }

  "factory:unfold" in {

    val dsu = DisjointSetUnion.unfold(1) { s =>
      if (s < 1000) Option((s, s * 2))
      else None
    }

    val checkUnfold = for {
      _ <- DisjointSetUnion.size.map(_ shouldBe 10)
      _ <- sequence(for (v <- 0 until dsu.size) yield {
        find[Int](v).map(_ shouldBe (1 << v)).map(_ => ())
      })
    } yield ()
    checkUnfold.run(dsu).value

  }

  "factory:concat" in {

    val xs = Vector(1, 2, 3)
    val ys = Vector(4, 5, 6)
    val dsu = DisjointSetUnion.concat(xs, ys)

    val checkConcat = for {
      _ <- DisjointSetUnion.size[Int].map(_ shouldBe (xs.size + ys.size))
      _ <- sequence(for (v <- xs.indices) yield {
        find[Int](v).map(_ shouldBe xs(v)).map(_ => ())
      })
      _ <- sequence(for (v <- ys.indices) yield {
        find[Int](xs.size + v).map(_ shouldBe ys(v)).map(_ => ())
      })
    } yield {
      ()
    }
    checkConcat.run(dsu).value

  }

  "factory:fill" in {

    val dsu = DisjointSetUnion.fill(100)(1)

    val checkFill = for {
      _ <- DisjointSetUnion.size.map(_ shouldBe 100)
      _ <- sequence(for (v <- 0 until dsu.size) yield {
        find[Int](v).map(_ shouldBe 1).map(_ => ())
      })
    } yield ()
    checkFill.run(dsu).value

  }

  "factory:tabulate" in {

    val dsu = DisjointSetUnion.tabulate(100)(identity)

    val checkTabulate = for {
      _ <- DisjointSetUnion.size.map(_ shouldBe 100)
      _ <- sequence(for (v <- 0 until dsu.size) yield {
        find[Int](v).map(_ shouldBe v).map(_ => ())
      })
    } yield ()
    checkTabulate.run(dsu).value

  }

  "case:simple" in {

    val dsu = DisjointSetUnion.fill(10)(1)

    val requires = sequence(
      for {
        u <- 0 until dsu.size
        v <- 0 until dsu.size
      } yield {
        val expectedSame = u == v
        isSame[Int](u, v).map(_ shouldBe expectedSame).map(_ => ())
      }
    )

    val updates =
      for {
        _ <- unite[Int](2, 3)
        _ <- unite(4, 5)
        _ <- unite(7, 8)
        _ <- unite(1, 3)
      } yield ()

    val expectedGroup = Vector(0, 1, 1, 1, 2, 2, 3, 4, 4, 5)
    val groupAssertions = sequence(
      for {
        u <- 0 until dsu.size
        v <- 0 until dsu.size
      } yield {
        val expectedSameGroup = expectedGroup(u) == expectedGroup(v)
        isSame[Int](u, v).map(_ shouldBe expectedSameGroup).map(_ => ())
      }
    )

    val expectedValue = Vector(1, 3, 3, 3, 2, 2, 1, 2, 2, 1)
    val valueAssertions = sequence(
      for (u <- 0 until dsu.size) yield {
        find[Int](u).map(_ shouldBe expectedValue(u)).map(_ => ())
      }
    )

    val app = for {
      _ <- requires
      _ <- updates
      _ <- groupAssertions
      _ <- valueAssertions
    } yield ()
    app.run(dsu).value

  }

}