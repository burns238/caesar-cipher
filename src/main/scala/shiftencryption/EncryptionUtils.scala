package shiftencryption

object EncryptionUtils {

	val Alphabet = " ABCDEFGHIJKLMNOPQRSTUVWXYZ"

	def shiftPosition(char: Character, key: Int): Int = {
		val offset = (key % Alphabet.length) + Alphabet.length
		val posWithOffset = Alphabet.indexOfSlice(char.toString) + offset
		posWithOffset % Alphabet.length
	}

	def shiftText(text: String, key: List[Int]): String = {

		val processedCharacters = for {
			group <- text.toUpperCase.grouped(key.length).toList
			(char, i) <- group.zipWithIndex
		} yield Alphabet.apply(shiftPosition(char, key(i)))

		processedCharacters.mkString
	}

	def encryptionKeyIntegersFromString(key: String): List[Int] = {
		key.toUpperCase.map(char => EncryptionUtils.Alphabet.indexOfSlice(char.toString)).toList
	}

	def decryptionKeyIntegersFromString(key: String): List[Int] = {
		key.toUpperCase.map(char => -(EncryptionUtils.Alphabet.indexOfSlice(char.toString))).toList
	}

}




