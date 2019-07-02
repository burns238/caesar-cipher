package dataencryptionstandard

import org.scalatest._
import dataencryptionstandard.BitVectorHelper._
import dataencryptionstandard.BitMappings._
import dataencryptionstandard.BitFunctions._

class SboxSpec extends FlatSpec with Matchers {

  "mapByteUsingSbox" should "return the correct binary value for sbox1 and 0000000" in {
    mapByteUsingSbox(createBitVector("000000"), Sbox1) shouldEqual createBitVector("1110")
  }

  it should "return the correct binary value for sbox1 and 00110010" in {
    mapByteUsingSbox(createBitVector("011001"), Sbox1) shouldEqual createBitVector("1001")
  }

  it should "return the correct binary value for sbox1 and 01001000" in {
    mapByteUsingSbox(createBitVector("100100"), Sbox1) shouldEqual createBitVector("1110")
  }

  it should "return the correct binary value for sbox1 and 11111111" in {
    mapByteUsingSbox(createBitVector("111111"), Sbox1) shouldEqual createBitVector("1101")
  }

}
