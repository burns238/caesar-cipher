package dataencryptionstandard

import org.scalatest._
import dataencryptionstandard.Sbox._
import dataencryptionstandard.BitVectorHelper._
import dataencryptionstandard.BitMappings._

class SboxSpec extends FlatSpec with Matchers {

  "getSboxElement" should "get element zero for implicit round 1" in {
    implicit val roundNumber = 1
    getSboxElement shouldEqual 0
  }

  it should "get element 1 for implicit round 2" in {
    implicit val roundNumber = 2
    getSboxElement shouldEqual 1
  }

  it should "get element 2 for implicit round 3" in {
    implicit val roundNumber = 3
    getSboxElement shouldEqual 2
  }

  it should "get element 7 for implicit round 8" in {
    implicit val roundNumber = 8
    getSboxElement shouldEqual 7
  }

  it should "get element 0 for implicit round 9" in {
    implicit val roundNumber = 9
    getSboxElement shouldEqual 0
  }

  it should "get element 1 for implicit round 10" in {
    implicit val roundNumber = 10
    getSboxElement shouldEqual 1
  }

  it should "get element 7 for implicit round 16" in {
    implicit val roundNumber = 16
    getSboxElement shouldEqual 7
  }

  "mapByteUsingSbox" should "return the correct binary value for sbox1 and 0000000" in {
    implicit val sbox = Sbox1
    mapByteUsingSbox(createBitVector("00000000")) shouldEqual createBitVector("00001110")
  }

  it should "return the correct binary value for sbox1 and 00110010" in {
    implicit val sbox = Sbox1
    mapByteUsingSbox(createBitVector("00110010")) shouldEqual createBitVector("00001001")
  }

  it should "return the correct binary value for sbox1 and 01001000" in {
    implicit val sbox = Sbox1
    mapByteUsingSbox(createBitVector("01001000")) shouldEqual createBitVector("00001110")
  }

  it should "return the correct binary value for sbox1 and 11111111" in {
    implicit val sbox = Sbox1
    mapByteUsingSbox(createBitVector("11111111")) shouldEqual createBitVector("00001101")
  }

  "applySbox" should "map all bits correctly for sbox1 for 0000 0000 0011 0010" in {
    implicit val round = 1
    applySbox(createBitVector("0000000000110010")) shouldEqual createBitVector("0000111000001001")
  }

  it should "map all bits correctly for sbox1 for 0100100011111111" in {
    implicit val round = 1
    applySbox(createBitVector("0100100011111111")) shouldEqual createBitVector("0000111000001101")
  }

}
