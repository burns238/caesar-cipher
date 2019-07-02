package dataencryptionstandard

import org.scalatest._
import dataencryptionstandard.BitFunctionsSpecHelper._
import dataencryptionstandard.BitFunctions._

import scala.util.Success

class BitFunctionsSpec extends FlatSpec with Matchers {

  "swap" should "for a 64 bit vector, take the first 32 and move them after the second 32" in {
    BitFunctions.swap(SixtyFourBits) shouldEqual Success(SixtyFourBitsSwapped)
  }

  "split" should "for a 64 bit vector, return the two halves in a tuple" in {
    BitFunctions.split(SixtyFourBits) shouldEqual Success(ThirtyTwoBitsTuple)
  }

  "stringToBits" should "convert a single character to a vector representing its bits" in {
  	BitFunctions.stringToBits(AString) shouldEqual Success(ABits)
  }

  it should "convert a short word to a vector representing its bits" in {
    BitFunctions.stringToBits(MikeString) shouldEqual Success(MikeBits)
  }

  it should "convert a sentence to a vector representing its bits" in {
    BitFunctions.stringToBits(LoremIpsum) shouldEqual Success(LoremIpsumBits)
  }

  "bitsToString" should "convert bits representing a character to its string representation" in {
    BitFunctions.bitsToString(ABits) shouldEqual AString
  }

  it should "convert bits representing a short word to its string representation" in {
    BitFunctions.bitsToString(MikeBits) shouldEqual MikeString
  }

  it should "convert bits representing a sentence to its string representation" in {
    BitFunctions.bitsToString(LoremIpsumBits) shouldEqual LoremIpsum
  } 

  "stringToBits followed by bitsToString" should "return the original string" in {
    BitFunctions.stringToBits(LoremIpsum).map(BitFunctions.bitsToString(_)) shouldEqual Success(LoremIpsum)
  }

  "leftShift" should "move all bits two to the left where the distance is 2" in {
    val bits = Vector('0','0','1','1')
    val expectedBits = Vector('1','1','0','0')
    BitFunctions.leftShift(bits, 2) shouldEqual Success(expectedBits)
  }

  it should "move all bits two to the left where the distance is 3" in {
    val bits = Vector('0','0','1','1')
    val expectedBits = Vector('1','0','0','1')
    BitFunctions.leftShift(bits, 3) shouldEqual Success(expectedBits)
  }

  it should "move all bits ten to the left where the distance is 10" in {
    val bits = Vector('0','0','1','1','0','0','1','1','0','0','1','1','0','0','1','1')
    val expectedBits = Vector('1','1','0','0','1','1','0','0','1','1','0','0','1','1','0','0')
    BitFunctions.leftShift(bits, 10) shouldEqual Success(expectedBits)
  }

  "xor" should "perform xor on two 8-bit vectors" in {
    val bits1 = Vector('0','0','1','0','0','1','0','0')
    val bits2 = Vector('0','0','1','0','0','0','0','1')
    val xorExpectedResult = 
                Vector('0','0','0','0','0','1','0','1')
    xor(bits1, bits2) shouldEqual Success(xorExpectedResult)
  }  

  it should "perform xor on two 32-bit vectors" in {
    xor(FirstThirtyTwoBits, SecondThirtyTwoBits) shouldEqual Success(FirstAndSecondThirtyTwoBitsXorResult)
  }

  it should "perform xor on two 48-bit vectors" in {
    xor(FirstFourtyEightBit, SecondFourtyEightBit) shouldEqual Success(FirstAndSecondFourtyEightBitXorResult)
  }

}
