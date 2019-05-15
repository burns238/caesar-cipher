package caesar

import scala.math.abs

object CaesarEncryption {
	
	val Alphabet = " ABCDEFGHIJKLMNOPQRSTUVWXYZ"

	def encryptText(plainText: String, key: Int): String = {
		processText(plainText, key)
	}

	def decryptText(cipherText: String, key: Int): String = {
		processText(cipherText, -key)
	}

	private def processText(text: String, key: Int): String = {
		text.toUpperCase.map(char => Alphabet.apply(getNewPosition(char, key)))
	}

	private def getNewPosition(char: Character, key: Int): Int = {
		val offset = (key % Alphabet.length) + Alphabet.length
		val posWithOffset = Alphabet.indexOfSlice(char.toString) + offset
		posWithOffset % Alphabet.length
	}

}