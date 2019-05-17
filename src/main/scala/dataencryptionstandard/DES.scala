package dataencryptionstandard

import dataencryptionstandard.BitFunctions._
import dataencryptionstandard.Permutation._
import dataencryptionstandard.SubkeyFunctions._
import dataencryptionstandard.Round._

object DES {
	
	private def crypt(plainText: String, key: String, initialPerm: Vector[Char] => Vector[Char], finalPerm: Vector[Char] => Vector[Char]): String = {
		if (key.length != 8) {
			throw new Exception("Key must be 8 chars long")
		}

		val (leftBits, rightBits) = (initialPerm andThen split64)(stringToBits(plainText))
		val subKey = permutedChoice1(stringToBits(key))

		val postRoundsBits = round(leftBits, rightBits, subKey, 1)
		val finalBits = (swap _ andThen finalPerm)(postRoundsBits)

		bitsToString(finalBits)
	}

	def encrypt(plainText: String, key: String): String = {
		crypt(plainText, key, initialPermutation, inversePermutation)
	}

	def decrypt(plainText: String, key: String): String = {
		crypt(plainText, key, inversePermutation, initialPermutation)
	}

}