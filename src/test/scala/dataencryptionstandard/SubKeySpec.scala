package dataencryptionstandard

import org.scalatest._

import scala.util.Success

class SubKeySpec extends FlatSpec with Matchers {

  "Permutate" should "reverse the bit positions where the permutation is reversal" in {
    val permutation = Vector(4,3,2,1)
    val bits = Vector('0','0','1','1')
    BitFunctions.permutate(bits, permutation) shouldEqual Success(Vector('1','1','0','0'))
  }
  
}