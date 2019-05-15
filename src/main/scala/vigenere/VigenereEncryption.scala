package vigenere

object VigenereEncryption {

	val Alphabet = " ABCDEFGHIJKLMNOPQRSTUVWXYZ"

	def encryptText(plainText: String, key: String): String = {
		processText(plainText, key, false)
	}

	def decryptText(cipherText: String, key: String): String = {
		processText(cipherText, key, true)
	}

	private def processText(text: String, key: String, decryption: Boolean): String = {

		val keyIntegers = key.toUpperCase.map{char => 
			val index = Alphabet.indexOfSlice(char.toString)
			if (decryption) -index else index
		}

		val processedCharacters = for {
			group <- text.toUpperCase.grouped(key.length).toList
			(char, i) <- group.zipWithIndex
		} yield Alphabet.apply(getNewPosition(char, keyIntegers(i)))

		processedCharacters.mkString
	}

	private def getNewPosition(char: Character, key: Int): Int = {
		val offset = (key % Alphabet.length) + Alphabet.length
		val posWithOffset = Alphabet.indexOfSlice(char.toString) + offset
		posWithOffset % Alphabet.length
	}

}