package lsh

import scala.util.Random

/**
 * Created with IntelliJ IDEA.
 * User: coderh
 * Date: 11/14/14
 * Time: 12:12 PM
 */
object MinHash {

    type HashFunction = Int => Int

    def generateHashFunctions(universeSize: Int, signatureSize: Int): Array[HashFunction] = {

      Array.tabulate(signatureSize) {
        i =>
          hashFunction(1 + 2 * Random.nextInt(1000), Random.nextInt(1000), universeSize)
      }
    }

    def hashFunction(a: Int, b: Int, uz: Int): HashFunction = {
      x: Int => (a * x + b) % uz
    }

}
