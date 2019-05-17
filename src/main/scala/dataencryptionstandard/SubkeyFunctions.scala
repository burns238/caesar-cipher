package dataencryptionstandard

object SubkeyFunctions {
	
	def reduceFrom64to56(bits: Vector[Char]): Vector[Char] = {
		bits.zipWithIndex.filter(z => (z._2+1) % 8 != 0).map(_._1)
	}

	def permutedChoice1(bits: Vector[Char]): (Vector[Char], Vector[Char]) = {
		(DESFunctions.permutate(bits, DESBitMappings.PermutedChoice1Left),
		 DESFunctions.permutate(bits, DESBitMappings.PermutedChoice1Right))	
	}
	
}