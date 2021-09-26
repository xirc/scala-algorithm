package algo.testing

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

abstract class BaseSpec
    extends AnyWordSpecLike
    with TypeCheckedTripleEquals
    with Matchers
    with Inside
