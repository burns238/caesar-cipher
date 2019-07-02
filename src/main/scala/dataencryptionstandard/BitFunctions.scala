package dataencryptionstandard

import dataencryptionstandard.BitMappings.{Expansion, FinalPermutation, InitialPermutation, PermutedChoice1, PermutedChoice2, RoundPermutation}
import dataencryptionstandard.Round._

import scala.annotation.tailrec
import scala.util.{Success, Try}

object BitFunctions {

	val InitialRound = 1

	def cryptChunk(plainText: String, subKeys: Try[Seq[Vector[Char]]], index: Int): Try[(String, Int)] =
    for {
      bits    <- stringToBits(plainText)
      init    <- initialPermutation(bits)
			(l, r) 	<- split(init)
      newBits <- round(l, r, subKeys, InitialRound)
      swapped <- swap(newBits)
      inverse <- inversePermutation(swapped)
    } yield (bitsToString(inverse), index)
	
	def stringToBits(text: String): Try[Vector[Char]] =
		Try((for {
			char  <- text
			bit   <- padLeftZeros(char.toInt.toBinaryString, 8)
		} yield bit).toVector)

	def bitsToString(bits: Vector[Char]): String = bitsToBytes(bits).mkString

	//The vector of chars passed in reflects the bits, the chars passed out are actually characters
	private def bitsToBytes(bits: Vector[Char]): Vector[Char] = {
		val bytes = bits.grouped(8).map(_.mkString).toVector
		bytes.map(b => Integer.parseInt(b, 2).toChar)
	}

	def padLeftZeros(text: String, length: Int): String = text.reverse.padTo(length, '0').reverse

	@tailrec
	def leftShift(bits: Vector[Char], distance: Int): Try[Vector[Char]] =
		distance match {
			case 0 => Success(bits)
			case d => leftShift(bits.tail :+ bits.head, d-1)
		}

	def swap(bits: Vector[Char]): Try[Vector[Char]] =
		split(bits).map(tup => tup._2 ++ tup._1)

	def split(bits: Vector[Char]): Try[(Vector[Char], Vector[Char])] = Try {
		val grouped = bits.grouped(bits.length/2).toVector
		(grouped(0), grouped(1))
	}

	def xor(bits1: Vector[Char], bits2: Vector[Char]): Try[Vector[Char]] =
		Try(for {
			(b1, b2)  <- bitsToBytes(bits1) zip bitsToBytes(bits2)
			c         <- padLeftZeros((b1 ^ b2).toBinaryString, 8)
		} yield c)

	def permutate(bits: Vector[Char], permutation: Vector[Int]): Try[Vector[Char]] =
		Try(permutation.map(v => bits.apply(v-1)))

	def expand(bits: Vector[Char]): Try[Vector[Char]] = permutate(bits, Expansion)
	def roundPermutation(bits: Vector[Char]): Try[Vector[Char]] = permutate(bits, RoundPermutation)
	def initialPermutation(bits: Vector[Char]): Try[Vector[Char]] = permutate(bits, InitialPermutation)
	def inversePermutation(bits: Vector[Char]): Try[Vector[Char]] = permutate(bits, FinalPermutation)
	def permutedChoice1(bits: Vector[Char]): Try[Vector[Char]] = permutate(bits, PermutedChoice1)
	def permutedChoice2(bits: Vector[Char]): Try[Vector[Char]] = permutate(bits, PermutedChoice2)

}