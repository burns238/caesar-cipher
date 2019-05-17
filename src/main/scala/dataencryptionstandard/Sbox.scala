package dataencryptionstandard

import dataencryptionstandard.BitMappings._
import dataencryptionstandard.BitFunctions._

object Sbox {
	
	def applySbox(bits: Vector[Char])(implicit roundNumber: Int): Vector[Char] = {

		implicit val sbox: Vector[Vector[Int]] = Sboxs(getSboxElement)

		bits.grouped(8).map(mapByteUsingSbox(_)).toVector.flatten
	}

	def mapByteUsingSbox(bits: Vector[Char])(implicit sbox: Vector[Vector[Int]]): Vector[Char] = {
		val firstDimension = intFromBits(Vector(bits(1),bits(6)))
		val secondDimension = intFromBits(Vector(bits(2),bits(3),bits(4),bits(5)))
		val thatIntFromSbox = sbox(firstDimension)(secondDimension)
		padLeftZeros(thatIntFromSbox.toBinaryString, 8).toVector
	}

	private def intFromBits(bits: Vector[Char]) = Integer.parseInt(bits.mkString, 2)

	def getSboxElement(implicit roundNumber: Int) = (roundNumber + (Sboxs.length - 1)) % Sboxs.length

}