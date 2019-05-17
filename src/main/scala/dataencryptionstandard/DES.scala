package dataencryptionstandard

object DES {
	
	def encrypt(plainText: String, key: String): String = {
		val plainTextBits: Vector[Char] = BitFunctions.stringToBits(plainText)
		//val permutatedBits = DESFunctions.initialPermutation(plainText)
		// val reasonablyEncryptedBits = DESFunctions.sixteenRounds(permutatedBits, key)
		// val swappedBits = DESFunctions.thirtyTwoBitSwap(reasonablyEncryptedBits)
		// DESFunctions.finalPermutation(swappedBits)
		???
	}

}