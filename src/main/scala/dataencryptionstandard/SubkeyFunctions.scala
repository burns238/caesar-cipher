package dataencryptionstandard

import dataencryptionstandard.BitMappings._
import dataencryptionstandard.BitFunctions._

import scala.util.{Success, Try}

object SubkeyFunctions {

	def leftShiftKey(subKey: Vector[Char], roundNumber: Int): Try[Vector[Char]] =
		for {
			(l, r)	<- split(subKey)
			a				<- leftShift(l, Shifts(roundNumber-1))
			b				<- leftShift(r, Shifts(roundNumber-1))
		} yield a ++ b

	def generateSubKeys(key: String): Try[Seq[Vector[Char]]] = {
		val InitialRound = 1
		for {
			bits			<- stringToBits(key)
			permuted	<- permutate(bits, PermutedChoice1)
			keys			<- generateKeys(permuted, Seq(), InitialRound)
		} yield keys
	}

	private def generateKeys(initialKey: Vector[Char], subKeys: Seq[Vector[Char]], count: Int): Try[Seq[Vector[Char]]] =
		count match {
			case 17 => Success(subKeys)
			case c =>
				for {
					shiftedKey	<- leftShiftKey(initialKey, c)
					newSubKey		<- permutate(shiftedKey, PermutedChoice2)
					keys				<- generateKeys(shiftedKey, subKeys :+ newSubKey, c+1)
				} yield keys
		}

}