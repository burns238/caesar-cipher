package dataencryptionstandard

object DESFunctions {
	
	def xor(left: Char, right: Char): Char = {
		(left ^ right).toChar
	}

	def permutate(bits: Vector[Char], permutation: Vector[Int]): Vector[Char] = {
		permutation.map(v => bits.apply(v-1))
	}

}