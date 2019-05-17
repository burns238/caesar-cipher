package dataencryptionstandard

import dataencryptionstandard.Permutation._
import dataencryptionstandard.BitMappings._
import dataencryptionstandard.BitFunctions._

object SubkeyFunctions {
	
	def permutedChoice1(bits: Vector[Char]): Vector[Char] = {
		permutate(bits, PermutedChoice1)
	}

	def permutedChoice2(subKey: Vector[Char], roundCount: Int): Vector[Char] = {
		val (l,r) = split56(subKey)
		val shifted = leftShift(l, Shifts(roundCount-1)) ++ leftShift(r, Shifts(roundCount-1))
		permutate(shifted, PermutedChoice2)
	} 

	def xorWithSubKey(bits: Vector[Char])(implicit subKey: Vector[Char]): Vector[Char] = {
		xor(bits, subKey)
	}	
}