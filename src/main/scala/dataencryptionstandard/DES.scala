package dataencryptionstandard

import dataencryptionstandard.BitFunctions._
import dataencryptionstandard.Permutation._
import dataencryptionstandard.SubkeyFunctions._
import dataencryptionstandard.Round._

object DES {
	
	private def crypto(plainText: String, subKeys: Seq[Vector[Char]]): String = {
		val (leftBits, rightBits) = (initialPermutation andThen split64)(stringToBits(plainText))
		val postRoundsBits = round(leftBits, rightBits, subKeys, 1)
		val finalBits = (swap _ andThen inversePermutation)(postRoundsBits)
		bitsToString(finalBits)
	}

	def encrypt(plainText: String, key: String): String = {
		val subKeys = generateSubKeys(key)
		crypto(plainText, subKeys)
	}

	//To decrypt, we use the same algorithm but reverse the subkeys we generate from the key passed in
	def decrypt(plainText: String, key: String): String = {
		val subKeys = generateSubKeys(key).reverse
		crypto(plainText, subKeys)
	}

}