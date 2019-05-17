package dataencryptionstandard

import dataencryptionstandard.BitFunctions._

object BitVectorHelper {

  def createBitVector(bits: Int, length: Int): Vector[Char] = {
    padLeftZeros(bits.toBinaryString, length).toVector
  }

  def createBitVector(bits: String): Vector[Char] = {
      bits.toVector
  }

}