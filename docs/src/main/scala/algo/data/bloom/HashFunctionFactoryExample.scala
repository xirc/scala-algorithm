package algo.data.bloom

object HashFunctionFactoryExample extends App {

  // #implementing_hash_function_factory
  implicit def customHashFunctionFactory[T]: HashFunctionFactory[T] =
    new HashFunctionFactory[T] {
      override def apply(index: Int): T => Int = {
        // Return a hash function for the given index.
        ???
      }
    }
  // #implementing_hash_function_factory

}
