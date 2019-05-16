package dataencryptionstandard

object DESFunctions {
	
	def leftShift(toBeShifted: Char, distance: Int): Char = {
		(toBeShifted >> distance).toChar
	}

	def xor(left: Char, right: Char): Char = {
		(left ^ right).toChar
	}

	def permutate(bits: Vector[Char], permutation: Vector[Int]): Vector[Char] = {
		permutation.map(v => bits.apply(v-1))
	}

	def reduceFrom64to56(bits: Vector[Char]): Vector[Char] = {
		bits.zipWithIndex.filter(z => (z._2+1) % 8 != 0).map(_._1)
	}

	def permutedChoice1(bits: Vector[Char]): (Vector[Char], Vector[Char]) = {
		(permutate(bits, DESBitMappings.PermutedChoice1Left),
		 permutate(bits, DESBitMappings.PermutedChoice1Right))	
	}

	def permutedChoice2(leftKey: Vector[Char], rightKey: Vector[Char]): Vector[Char] = {
		???
	}

}