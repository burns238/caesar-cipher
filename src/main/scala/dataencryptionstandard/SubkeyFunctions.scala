package dataencryptionstandard

import dataencryptionstandard.BitMappings._
import dataencryptionstandard.BitFunctions._

import scala.util.Try

import cats.implicits._

object SubkeyFunctions {

	def generateSubKeys(key: String): Try[Seq[Vector[Char]]] = {
		val InitialRound = 1
		for {
			bits			<- stringToBits(key)
			permuted	<- permutate(bits, PermutedChoice1)
			keys			<- generateKeys(permuted, Seq(), InitialRound)
		} yield keys
	}

	private def generateKeys(initialKey: Vector[Char], subKeys: Seq[Vector[Char]], count: Int): Try[Vector[Vector[Char]]] =
		Shifts.map(s =>
			for {
				shiftedKey	<- leftShiftKey(initialKey, s)
				newSubKey		<- permutate(shiftedKey, PermutedChoice2)
			} yield newSubKey
		).sequence

	def leftShiftKey(subKey: Vector[Char], shift: Int): Try[Vector[Char]] =
		for {
			(l, r)	<- split(subKey)
			a				<- leftShift(l, shift)
			b				<- leftShift(r, shift)
		} yield a ++ b

}