package algo.data.dsu.mutable

import algo.testing.BaseSpec

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

  "apply" in {

    inside(DisjointSetUnion.fill(100)(1)) { dsu =>
      for (v <- 0 until 100) {
        assert(dsu(v) === 1)
      }
    }

    inside(DisjointSetUnion.tabulate(100)(identity)) { dsu =>
      for (v <- 0 until 100) {
        assert(dsu(v) === v)
      }
    }

    inside(DisjointSetUnion.fill(10)(1)) { dsu =>
      intercept[IndexOutOfBoundsException] {
        dsu(-1)
      }
      intercept[IndexOutOfBoundsException] {
        dsu(dsu.size)
      }
    }

  }

  "find" in {

    inside(DisjointSetUnion.fill(100)(1)) { dsu =>
      for (v <- 0 until 100) {
        assert(dsu.find(v) === 1)
      }
    }

    inside(DisjointSetUnion.tabulate(100)(identity)) { dsu =>
      for (v <- 0 until 100) {
        assert(dsu.find(v) === v)
      }
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

    for {
      u <- 0 until dsu.size
      v <- 0 until dsu.size
    } {
      if (u == v) {
        assert(dsu.isSame(u, v))
      } else {
        assert(!dsu.isSame(u, v))
      }
    }

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
    dsu.unite(1, 3)
    assert(dsu.isSame(1, 3))
    assert(dsu(1) === dsu(3))

    // Unite elements in same group
    dsu.unite(3, 1)
    assert(dsu.isSame(1, 3))

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

  "factory:from" in {

    val xs = Vector(1, 2, 3, 4, 5)
    val dsu = DisjointSetUnion.from(xs.iterator)
    assert(dsu.size === 5)
    for (v <- 0 until dsu.size) {
      assert(dsu(v) === (v + 1))
    }

  }

  "factory:apply" in {

    val dsu = DisjointSetUnion(1, 2, 3, 4, 5)
    assert(dsu.size === 5)
    for (v <- 0 until dsu.size) {
      assert(dsu(v) === (v + 1))
    }

  }

  "factory:iterate" in {

    val dsu = DisjointSetUnion.iterate(1, 10)(_ * 2)
    assert(dsu.size === 10)
    for (v <- 0 until dsu.size) {
      assert(dsu(v) === (1 << v))
    }

  }

  "factory:unfold" in {

    val dsu = DisjointSetUnion.unfold(1) { s =>
      if (s < 1000) Option((s, s * 2))
      else None
    }
    assert(dsu.size === 10)
    for (v <- 0 until dsu.size) {
      assert(dsu(v) === (1 << v))
    }

  }

  "factory:concat" in {

    val xs = Vector(1, 2, 3)
    val ys = Vector(4, 5, 6)
    val dsu = DisjointSetUnion.concat(xs, ys)
    assert(dsu.size === (xs.size + ys.size))
    for (v <- xs.indices) {
      assert(dsu(v) === xs(v))
    }
    for (v <- ys.indices) {
      assert(dsu(xs.size + v) === ys(v))
    }

  }

  "factory:fill" in {

    val dsu = DisjointSetUnion.fill(100)(1)
    assert(dsu.size === 100)
    for (v <- 0 until dsu.size) {
      assert(dsu(v) === 1)
    }

  }

  "factory:tabulate" in {

    val dsu = DisjointSetUnion.tabulate(100)(identity)
    assert(dsu.size === 100)
    for (v <- 0 until dsu.size) {
      assert(dsu(v) === v)
    }

  }

  "case:simple" in {

    val dsu = DisjointSetUnion.fill(10)(1)

    for {
      u <- 0 until dsu.size
      v <- 0 until dsu.size
    } {
      if (u == v) {
        assert(dsu.isSame(u, v))
      } else {
        assert(!dsu.isSame(u, v))
      }
    }

    dsu
      .unite(2, 3)
      .unite(4, 5)
      .unite(7, 8)
      .unite(1, 3)

    val expectedGroup = Vector(0, 1, 1, 1, 2, 2, 3, 4, 4, 5)
    for {
      u <- 0 until dsu.size
      v <- 0 until dsu.size
    } {
      val expectedSameGroup = expectedGroup(u) == expectedGroup(v)
      assert(dsu.isSame(u, v) === expectedSameGroup)
    }

    val expectedValue = Vector(1, 3, 3, 3, 2, 2, 1, 2, 2, 1)
    for (u <- 0 until dsu.size) {
      assert(dsu(u) === expectedValue(u))
    }

  }

}
