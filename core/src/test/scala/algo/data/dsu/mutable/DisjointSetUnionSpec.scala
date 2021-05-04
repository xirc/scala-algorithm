package algo.data.dsu.mutable

import algo.testing.BaseSpec

final class DisjointSetUnionSpec extends BaseSpec {

  "size" in {

    DisjointSetUnion.fill(1)(1).size shouldBe 1
    DisjointSetUnion.fill(10)(1).size shouldBe 10
    DisjointSetUnion.fill(100)(1).size shouldBe 100
    DisjointSetUnion.fill(1_000)(1).size shouldBe 1_000
    DisjointSetUnion.fill(10_000)(1).size shouldBe 10_000
    DisjointSetUnion.fill(100_000)(1).size shouldBe 100_000
    DisjointSetUnion.fill(1_000_000)(1).size shouldBe 1_000_000

  }

  "apply" in {

    inside(DisjointSetUnion.fill(100)(1)) { dsu =>
      for (v <- 0 until 100) {
        dsu(v) shouldBe 1
      }
    }

    inside(DisjointSetUnion.tabulate(100)(identity)) { dsu =>
      for (v <- 0 until 100) {
        dsu(v) shouldBe v
      }
    }

    inside(DisjointSetUnion.fill(10)(1)) { dsu =>
      a[IndexOutOfBoundsException] shouldBe thrownBy {
        dsu(-1)
      }
      a[IndexOutOfBoundsException] shouldBe thrownBy {
        dsu(dsu.size)
      }
    }

  }

  "find" in {

    inside(DisjointSetUnion.fill(100)(1)) { dsu =>
      for (v <- 0 until 100) {
        dsu.find(v) shouldBe 1
      }
    }

    inside(DisjointSetUnion.tabulate(100)(identity)) { dsu =>
      for (v <- 0 until 100) {
        dsu.find(v) shouldBe v
      }
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

    for {
      u <- 0 until dsu.size
      v <- 0 until dsu.size
    } {
      if (u == v) {
        dsu.isSame(u, v) shouldBe true
      } else {
        dsu.isSame(u, v) shouldBe false
      }
    }

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
    dsu.unite(1, 3)
    dsu.isSame(1, 3) shouldBe true
    dsu(1) shouldBe dsu(3)

    // Unite elements in same group
    dsu.unite(3, 1)
    dsu.isSame(1, 3) shouldBe true

    a[IndexOutOfBoundsException] shouldBe thrownBy {
      dsu.unite(-1, 3)
    }

    a[IndexOutOfBoundsException] shouldBe thrownBy {
      dsu.unite(dsu.size, 3)
    }

    a[IndexOutOfBoundsException] shouldBe thrownBy {
      dsu.unite(-1, -1)
    }

    a[IndexOutOfBoundsException] shouldBe thrownBy {
      dsu.unite(1, dsu.size)
    }

  }

  "factory:from" in {

    val xs = Vector(1, 2, 3, 4, 5)
    val dsu = DisjointSetUnion.from(xs.iterator)
    dsu.size shouldBe 5
    for (v <- 0 until dsu.size) {
      dsu(v) shouldBe (v + 1)
    }

  }

  "factory:apply" in {

    val dsu = DisjointSetUnion(1, 2, 3, 4, 5)
    dsu.size shouldBe 5
    for (v <- 0 until dsu.size) {
      dsu(v) shouldBe (v + 1)
    }

  }

  "factory:iterate" in {

    val dsu = DisjointSetUnion.iterate(1, 10)(_ * 2)
    dsu.size shouldBe 10
    for (v <- 0 until dsu.size) {
      dsu(v) shouldBe (1 << v)
    }

  }

  "factory:unfold" in {

    val dsu = DisjointSetUnion.unfold(1) { s =>
      if (s < 1000) Option((s, s * 2))
      else None
    }
    dsu.size shouldBe 10
    for (v <- 0 until dsu.size) {
      dsu(v) shouldBe (1 << v)
    }

  }

  "factory:concat" in {

    val xs = Vector(1, 2, 3)
    val ys = Vector(4, 5, 6)
    val dsu = DisjointSetUnion.concat(xs, ys)
    dsu.size shouldBe (xs.size + ys.size)
    for (v <- xs.indices) {
      dsu(v) shouldBe xs(v)
    }
    for (v <- ys.indices) {
      dsu(xs.size + v) shouldBe ys(v)
    }

  }

  "factory:fill" in {

    val dsu = DisjointSetUnion.fill(100)(1)
    dsu.size shouldBe 100
    for (v <- 0 until dsu.size) {
      dsu(v) shouldBe 1
    }

  }

  "factory:tabulate" in {

    val dsu = DisjointSetUnion.tabulate(100)(identity)
    dsu.size shouldBe 100
    for (v <- 0 until dsu.size) {
      dsu(v) shouldBe v
    }

  }

  "case:simple" in {

    val dsu = DisjointSetUnion.fill(10)(1)

    for {
      u <- 0 until dsu.size
      v <- 0 until dsu.size
    } {
      if (u == v) {
        dsu.isSame(u, v) shouldBe true
      } else {
        dsu.isSame(u, v) shouldBe false
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
      dsu.isSame(u, v) shouldBe expectedSameGroup
    }

    val expectedValue = Vector(1, 3, 3, 3, 2, 2, 1, 2, 2, 1)
    for (u <- 0 until dsu.size) {
      dsu(u) shouldBe expectedValue(u)
    }

  }

}
