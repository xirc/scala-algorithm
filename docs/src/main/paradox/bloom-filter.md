# Bloom Filter

`algo.data.bloom` provides implementations of
@link:[Bloom filter](https://dl.acm.org/doi/10.1145/362686.362692) { open=new }, which can store a large amount of data
by allowing small errors. There are the two following implementations:

* @scaladoc[immutable.BloomFilter](algo.data.bloom.immutable.BloomFilter)
* @scaladoc[mutable.BloomFilter](algo.data.bloom.mutable.BloomFilter)

## Creating a BloomFilter instance

`BloomFilter.apply` method creates an instance of `BloomFilter` as the following:

Immutable
: @@snip [BloomFilterExample.scala](/docs/src/main/scala/algo/data/bloom/immutable/BloomFilterExample.scala) { #creating_instance }

Mutable
: @@snip [BloomFilterExample.scala](/docs/src/main/scala/algo/data/bloom/mutable/BloomFilterExample.scala) { #creating_instance }

`BloomFilter.apply` method takes two parameters capacity and tolerance. Further, `BloomFilter` also takes an instance of
@scaladoc[HashFunctionFactory](algo.data.bloom.HashFunctionFactory) as a context (implicit) parameter. These parameters
determine the characteristics of `BloomFilter`.

## Characteristics

Parameters the capacity and tolerance are tunable for a false positive probability. The false positive probability is
less than the tolerance if the number of items `BloomFilter` contains is less than or equal to the capacity. If a lower
false positive probability is required, use a higher capacity or a lower tolerance.

Based on the capacity and tolerance, `BloomFilter` determines hash space (the number of bits) and the number of hash
functions to use. `BloomFilter` requires a larger hash space and more hash functions if a higher capacity or lower
tolerance is requested. In other words, `BloomFilter` requires a larger hash space if it should achieve a lower false
positive probability.

Note that the capacity is not the hard limit of the number of items `BloomFilter` can contain. While `BloomFilter` can
contain more items than the capacity, it cannot achieve a lower false positive probability than the tolerance.

## Getting parameters

`BloomFilter` provides functions returning parameters the following:

* The capacity
* The tolerance
* The number of bits
* The number of hash functions

Immutable
: @@snip [BloomFilterExample.scala](/docs/src/main/scala/algo/data/bloom/immutable/BloomFilterExample.scala) { #getting_parameters }

Mutable
: @@snip [BloomFilterExample.scala](/docs/src/main/scala/algo/data/bloom/mutable/BloomFilterExample.scala) { #getting_parameters }

## Adding an item

`BloomFilter` provides `add` method to add the given item to itself:

Immutable
: @@snip [BloomFilterExample.scala](/docs/src/main/scala/algo/data/bloom/immutable/BloomFilterExample.scala) { #adding_item }

Mutable
: @@snip [BloomFilterExample.scala](/docs/src/main/scala/algo/data/bloom/mutable/BloomFilterExample.scala) { #adding_item }

`add` method of `immutable.BloomFilter` returns a new BloomFilter having the given item. `add` method
of `mutable.BloomFilter` updates itself to contain the given item. In both, `BloomFilter` increments its size by one if
it doesn't have the given item.

## Testing membership of an item

`BloomFilter` provides `contains` method to test whether `BloomFilter` contains the given item or not:

Immutable
: @@snip [BloomFilterExample.scala](/docs/src/main/scala/algo/data/bloom/immutable/BloomFilterExample.scala) { #testing_membership }

Mutable
: @@snip [BloomFilterExample.scala](/docs/src/main/scala/algo/data/bloom/mutable/BloomFilterExample.scala) { #testing_membership }

`contains` method returns true if `BloomFilter` contains the given item. Note that `BloomFilter` can return true if it
doesn't have the given item but has the other item with the same hash value, which is a false positive case. In
contrast, `BloomFilter` doesn't return false if it has the given item.

## Getting the size and false positive probability

`BloomFilter` provides `size` method to get the number of items it has:

Immutable
: @@snip [BloomFilterExample.scala](/docs/src/main/scala/algo/data/bloom/immutable/BloomFilterExample.scala) { #getting_size }

Mutable
: @@snip [BloomFilterExample.scala](/docs/src/main/scala/algo/data/bloom/mutable/BloomFilterExample.scala) { #getting_size }

`BloomFilter` also provides `falsePositiveProbability` method to get the current false positive probability:

Immutable
: @@snip [BloomFilterExample.scala](/docs/src/main/scala/algo/data/bloom/immutable/BloomFilterExample.scala) { #getting_false_positive_probability }

Mutable
: @@snip [BloomFilterExample.scala](/docs/src/main/scala/algo/data/bloom/mutable/BloomFilterExample.scala) { #getting_false_positive_probability }

## Hash Function Factory

`BloomFilter`, a probabilistic collection, hashes the given item using hash functions. The hash functions are crucial
for a false positive probability. `HashFunctionFactory` creates these hash functions.

`HashFunctionFactory.Implicits.default` provides the default implementation of `HashFunctionFactory`. This default is
easier to use but might not be a suitable performance. It is possible to implement a
custom `HashFunctionFactory` as the following:

@@snip [HashFunctionFactoryExample.scala](/docs/src/main/scala/algo/data/bloom/HashFunctionFactoryExample.scala) { #implementing_hash_function_factory }
