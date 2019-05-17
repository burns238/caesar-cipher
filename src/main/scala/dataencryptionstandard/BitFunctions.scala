package dataencryptionstandard

import scala.annotation.tailrec

object BitFunctions {
	
	def stringToBits(text: String): Vector[Char] = {
		(for {
			char <- text
			bit <- padLeftZeros(char.toInt.toBinaryString, 8)
		} yield bit).toVector
	}

	def bitsToString(bits: Vector[Char]): String = {
		bitsToBytes(bits).mkString
	}

	//The vector of chars passed in reflects the bits, the chars passed out are actually chars
	private def bitsToBytes(bits: Vector[Char]): Vector[Char] = {
		val bytes = bits.grouped(8).map(_.mkString).toVector
		bytes.map(b => Integer.parseInt(b, 2).toChar)
	}

	private def padLeftZeros(text: String, length: Int): String = {
		text.reverse.padTo(length, '0').reverse
	}

	@tailrec
	def leftShift(bits: Vector[Char], distance: Int): Vector[Char] = {
		distance match {
			case 0 => bits
			case d => leftShift(bits.tail :+ bits.head, d-1)
		}
	}

	def swap(bits: Vector[Char]): Vector[Char] = {
		val (left, right) = split64(bits)
		right ++ left
	}

	private def split(bits: Vector[Char], bitCount: Int): (Vector[Char], Vector[Char]) = {
		val grouped = bits.grouped(bitCount).toVector
		(grouped(0), grouped(1))
	}

	val split64 = (bits: Vector[Char]) => split(bits, 32)
	val split56 = (bits: Vector[Char]) => split(bits, 28)

	def xor(bits1: Vector[Char], bits2: Vector[Char]): Vector[Char] = {
		for {
			(b1, b2) <- bitsToBytes(bits1) zip bitsToBytes(bits2)
			c <- padLeftZeros((b1 ^ b2).toBinaryString, 8)
		} yield c
	}

	def sbox(bits: Vector[Char]): Vector[Char] = {

		???

		Vector()
	}

}