package algo.data.dsu.immutable

import algo.data.dsu.immutable.syntax.*
import algo.testing.BaseSpec
import cats.data.State
import cats.instances.all.*
import cats.syntax.foldable.*

final class DisjointSetUnionSpec extends BaseSpec {

  "size" in {

    assert(DisjointSetUnion.fill(1)(1).size === 1)
    assert(DisjointSetUnion.fill(10)(1).size === 10)
    assert(DisjointSetUnion.fill(100)(1).size === 100)
    assert(DisjointSetUnion.fill(1_000)(1).size === 1_000)
    assert(DisjointSetUnion.fill(10_000)(1).size === 10_000)
    assert(DisjointSetUnion.fill(100_000)(1).size === 100_000)
    assert(DisjointSetUnion.fill(1_000_000)(1).size === 1_000_000)

  }

  "find" in {

    inside(DisjointSetUnion.fill(100)(1)) { dsu =>
      val findSpec =
        (0 until 100)
          .map { index =>
            find[Int](index).map { value =>
              assert(value === 1)
            }
          }
          .toList
          .sequence_
      findSpec.run(dsu).value
    }

    inside(DisjointSetUnion.tabulate(100)(identity)) { dsu =>
      val findSpec =
        (0 until 100)
          .map { index =>
            find[Int](index).map { value =>
              assert(value === index)
            }
          }
          .toList
          .sequence_
      findSpec.run(dsu).value
    }

    inside(DisjointSetUnion.fill(10)(1)) { dsu =>
      intercept[IndexOutOfBoundsException] {
        dsu.find(-1)
      }
      intercept[IndexOutOfBoundsException] {
        dsu.find(dsu.size)
      }
    }

  }

  "isSame" in {

    val dsu = DisjointSetUnion.fill(10)(1)

    val isSameSpec =
      (for {
        u <- 0 until dsu.size
        v <- 0 until dsu.size
      } yield {
        val expectedSame = u == v
        isSame[Int](u, v)
          .map { same =>
            assert(same === expectedSame)
          }
      }).toList.sequence_
    isSameSpec.run(dsu).value

    intercept[IndexOutOfBoundsException] {
      dsu.isSame(-1, 1)
    }

    intercept[IndexOutOfBoundsException] {
      dsu.isSame(dsu.size, 1)
    }

    intercept[IndexOutOfBoundsException] {
      dsu.isSame(1, -1)
    }

    intercept[IndexOutOfBoundsException] {
      dsu.isSame(1, dsu.size)
    }

  }

  "unite" in {

    val dsu = DisjointSetUnion.fill(10)(1)

    val uniteSpec = for {
      _ <- unite[Int](1, 3)
      same <- isSame(1, 3)
      uValue <- find(1)
      vValue <- find(3)
    } yield {
      assert(same)
      assert(uValue === vValue)
    }
    uniteSpec.run(dsu).value

    val uniteSameGroupSpec = for {
      _ <- unite[Int](2, 4)
      s1 <- State.get
      // Unite same group
      _ <- unite[Int](4, 2)
      s2 <- State.get
    } yield {
      assert(s1 === s2)
    }
    uniteSameGroupSpec.run(dsu).value

    intercept[IndexOutOfBoundsException] {
      dsu.unite(-1, 3)
    }

    intercept[IndexOutOfBoundsException] {
      dsu.unite(dsu.size, 3)
    }

    intercept[IndexOutOfBoundsException] {
      dsu.unite(-1, -1)
    }

    intercept[IndexOutOfBoundsException] {
      dsu.unite(1, dsu.size)
    }

  }

  "factory|from" in {

    val xs = Vector(1, 2, 3, 4, 5)
    val dsu = DisjointSetUnion.from(xs.iterator)

    val fromSpec = for {
      _ <- size.map(size => assert(size === 5))
      _ <- (for (v <- 0 until dsu.size) yield {
        find[Int](v).map { value =>
          assert(value === (v + 1))
        }
      }).toList.sequence_
    } yield ()
    fromSpec.run(dsu).value

  }

  "factory|empty" in {

    val dsu = DisjointSetUnion.empty[Int]
    assert(dsu.size === 0)

  }

  "factory|apply" in {

    val dsu = DisjointSetUnion(1, 2, 3, 4, 5)

    val applySpec = for {
      _ <- size.map(size => assert(size === 5))
      _ <- (for (v <- 0 until dsu.size) yield {
        find[Int](v).map { value =>
          assert(value === (v + 1))
        }
      }).toList.sequence_
    } yield ()
    applySpec.run(dsu).value

  }

  "factory|iterate" in {

    val dsu = DisjointSetUnion.iterate(1, 10)(_ * 2)

    val iterateSpec = for {
      _ <- size.map(size => assert(size === 10))
      _ <- (for (v <- 0 until dsu.size) yield {
        find[Int](v).map { value =>
          assert(value === (1 << v))
        }
      }).toList.sequence_
    } yield ()
    iterateSpec.run(dsu).value

  }

  "factory|unfold" in {

    val dsu = DisjointSetUnion.unfold(1) { s =>
      if (s < 1000) Option((s, s * 2))
      else None
    }

    val unfoldSpec = for {
      _ <- size.map(size => assert(size === 10))
      _ <- (for (v <- 0 until dsu.size) yield {
        find[Int](v).map { value =>
          assert(value === (1 << v))
        }
      }).toList.sequence_
    } yield ()
    unfoldSpec.run(dsu).value

  }

  "factory|concat" in {

    val xs = Vector(1, 2, 3)
    val ys = Vector(4, 5, 6)
    val dsu = DisjointSetUnion.concat(xs, ys)

    val concatSpec = for {
      _ <- size[Int].map(size => assert(size === (xs.size + ys.size)))
      _ <- (for (v <- xs.indices) yield {
        find[Int](v).map { value =>
          assert(value === xs(v))
        }
      }).toList.sequence_
      _ <- (for (v <- ys.indices) yield {
        find[Int](xs.size + v).map { value =>
          assert(value === ys(v))
        }
      }).toList.sequence_
    } yield ()
    concatSpec.run(dsu).value

  }

  "factory|fill" in {

    val dsu = DisjointSetUnion.fill(100)(1)

    val fillSpec = for {
      _ <- size.map(size => assert(size === 100))
      _ <- (for (v <- 0 until dsu.size) yield {
        find[Int](v).map { value =>
          assert(value === 1)
          ()
        }
      }).toList.sequence_
    } yield ()
    fillSpec.run(dsu).value

  }

  "factory|tabulate" in {

    val dsu = DisjointSetUnion.tabulate(100)(identity)

    val tabulateSpec = for {
      _ <- size.map(size => assert(size === 100))
      _ <- (for (v <- 0 until dsu.size) yield {
        find[Int](v).map { value =>
          assert(value === v)
        }
      }).toList.sequence_
    } yield ()
    tabulateSpec.run(dsu).value

  }

  "factory|to" in {

    val dsu = Seq(1, 2, 3, 4, 5).to(DisjointSetUnion)
    assert(dsu.size === 5)
    val toSpec = (
      for (i <- 0 until 5) yield {
        find[Int](i).map { value =>
          assert(value === (i + 1))
        }
      }
    ).toList.sequence_
    toSpec.run(dsu).value

  }

  "factory|newBuilder" in {

    val builder = DisjointSetUnion.newBuilder[Int]
    builder.addOne(4)
    builder.clear()
    builder.addOne(1)
    builder.addAll(Seq(2, 3, 4, 5))

    val dsu = builder.result()
    assert(dsu.size === 5)
    val newBuilderSpec = (
      for (i <- 0 until 5) yield {
        find[Int](i).map { value =>
          assert(value === (i + 1))
        }
      }
    ).toList.sequence_
    newBuilderSpec.run(dsu).value

  }

  "case|simple" in {

    val dsu = DisjointSetUnion.fill(10)(1)

    val requiresSpec = (
      for {
        u <- 0 until dsu.size
        v <- 0 until dsu.size
      } yield {
        val expectedSame = u == v
        isSame[Int](u, v).map { same =>
          assert(same === expectedSame)
        }
      }
    ).toList.sequence_

    val updatesSpec =
      for {
        _ <- unite[Int](2, 3)
        _ <- unite(4, 5)
        _ <- unite(7, 8)
        _ <- unite(1, 3)
      } yield ()

    val expectedGroup = Vector(0, 1, 1, 1, 2, 2, 3, 4, 4, 5)
    val groupAssertionSpec = (
      for {
        u <- 0 until dsu.size
        v <- 0 until dsu.size
      } yield {
        val expectedSameGroup = expectedGroup(u) == expectedGroup(v)
        isSame[Int](u, v).map { same =>
          assert(same === expectedSameGroup)
        }
      }
    ).toList.sequence_

    val expectedValue = Vector(1, 3, 3, 3, 2, 2, 1, 2, 2, 1)
    val valueAssertionSpec = (
      for (u <- 0 until dsu.size) yield {
        find[Int](u).map { value =>
          assert(value === expectedValue(u))
        }
      }
    ).toList.sequence_

    val spec = for {
      _ <- requiresSpec
      _ <- updatesSpec
      _ <- groupAssertionSpec
      _ <- valueAssertionSpec
    } yield ()
    spec.run(dsu).value

  }

}
