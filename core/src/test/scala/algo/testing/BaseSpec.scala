package algo.testing

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Inside
import org.scalatest.wordspec.AnyWordSpecLike

abstract class BaseSpec
    extends AnyWordSpecLike
    with TypeCheckedTripleEquals
    with Inside
