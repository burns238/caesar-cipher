package encryption

object VigenereEncryption {

	def encryptText(plainText: String, key: String): String = {
		EncryptionUtils.shiftText(plainText, EncryptionUtils.encryptionKeyIntegersFromString(key))
	}

	def decryptText(cipherText: String, key: String): String = {
		EncryptionUtils.shiftText(cipherText, EncryptionUtils.decryptionKeyIntegersFromString(key))
	}

}