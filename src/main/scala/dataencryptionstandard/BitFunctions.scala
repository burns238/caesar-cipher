package dataencryptionstandard

import scala.annotation.tailrec

object BitFunctions {
	
	def stringToBits(text: String): Vector[Char] = {
		val bits: String = for {
			char <- text
			bit <- padLeftZeros(char.toInt.toBinaryString, 8)
		} yield bit

		bits.toVector
	}

	def bitsToString(bits: Vector[Char]): String = {
		val bytes = bits.grouped(8).map(_.mkString).toVector
		bytes.map(b => Integer.parseInt(b, 2).toChar).mkString;
	}

	def padLeftZeros(text: String, length: Int): String = {
		text.reverse.padTo(length, '0').reverse
	}

	@tailrec
	def leftShift(bits: Vector[Char], distance: Int): Vector[Char] = {
		distance match {
			case 0 => bits
			case d => leftShift(bits.tail :+ bits.head, d-1)
		}
	}

}