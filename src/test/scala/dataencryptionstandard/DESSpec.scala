package dataencryptionstandard

import org.scalatest._

class DESSpec extends FlatSpec with Matchers {

  "Permutate" should "reverse the bit positions where the permutation is reversal" in {
  	val permutation = Vector(4,3,2,1)
  	val bits = Vector('0','0','1','1')
  	DESFunctions.permutate(bits, permutation) shouldEqual Vector('1','1','0','0')
  }

  it should "return the same order where the permutation is in ascending order" in {
  	val permutation = Vector(1,2,3,4)
  	val bits = Vector('0','0','1','1')
  	DESFunctions.permutate(bits, permutation) shouldEqual bits
  }

  it should "return the order described by the permutation" in {
  	val permutation = Vector(1,4,3,1)
  	val bits = Vector('0','0','1','1')
  	DESFunctions.permutate(bits, permutation) shouldEqual Vector('0','1','1','0')
  }

  "reduceFrom64to56" should "remove the 8th and 16th characters in a 16 character vector" in {
  	val bits = Vector('0','0','1','1','0','0','1','1','0','0','1','1','0','0','1','1') 
  	val expectedBits = Vector('0','0','1','1','0','0','1','0','0','1','1','0','0','1') 
  	DESFunctions.reduceFrom64to56(bits) shouldEqual expectedBits
  }

  "reduceFrom64to56" should "remove the 8th, 16th, 24th and 32nd characters in a 32 character vector" in {
  	val bits = Vector('0','0','1','1','0','0','1','1','0','0','1','1','0','0','1','1',
                      '0','0','1','1','0','0','1','1','0','0','1','1','0','0','1','1') 
  	val expectedBits = Vector('0','0','1','1','0','0','1','0','0','1','1','0','0','1',
                              '0','0','1','1','0','0','1','0','0','1','1','0','0','1') 
  	DESFunctions.reduceFrom64to56(bits) shouldEqual expectedBits
  }

}
