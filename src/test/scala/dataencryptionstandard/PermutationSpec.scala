package dataencryptionstandard

import org.scalatest._

import scala.util.Success

class PermutationSpec extends FlatSpec with Matchers {

  "Permutate" should "reverse the bit positions where the permutation is reversal" in {
    val permutation = Vector(4,3,2,1)
    val bits = Vector('0','0','1','1')
    BitFunctions.permutate(bits, permutation) shouldEqual Success(Vector('1','1','0','0'))
  }

  it should "return the same order where the permutation is in ascending order" in {
    val permutation = Vector(1,2,3,4)
    val bits = Vector('0','0','1','1')
    BitFunctions.permutate(bits, permutation) shouldEqual Success(bits)
  }

  it should "return the order described by the permutation" in {
    val permutation = Vector(1,4,3,1)
    val bits = Vector('0','0','1','1')
    BitFunctions.permutate(bits, permutation) shouldEqual Success(Vector('0','1','1','0'))
  }
  
}