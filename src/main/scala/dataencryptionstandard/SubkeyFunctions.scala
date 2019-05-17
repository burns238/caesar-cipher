package dataencryptionstandard

import dataencryptionstandard.Permutation._
import dataencryptionstandard.BitMappings._
import dataencryptionstandard.BitFunctions._

object SubkeyFunctions {
	
	def permutedChoice1(bits: Vector[Char]): Vector[Char] = {
		permutate(bits, PermutedChoice1)
	}

	def permutedChoice2(subKey: Vector[Char])(implicit roundNumber: Int): Vector[Char] = {
		val (l,r) = split56(subKey)
		val shifted = leftShift(l, Shifts(roundNumber-1)) ++ leftShift(r, Shifts(roundNumber-1))
		permutate(shifted, PermutedChoice2)
	} 

}