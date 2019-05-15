package caesar

object Caesar extends App {

	val DefaultKey = 3

	def encrypt {
		println("Write text to encrypt")
		val plainText = scala.io.StdIn.readLine
		println(CaesarEncryption.encryptText(plainText, readKey))
	}

	def decrypt {
		println("Write text to decrypt")
		val plainText = scala.io.StdIn.readLine()
		println(CaesarEncryption.decryptText(plainText, readKey))
	}

	def readKey: Int = {
		println("Give an integer key (default 3)")
		try {
			scala.io.StdIn.readLine().toInt
		} catch {
			case e: Exception => DefaultKey
		}
	}

	println("Encrypt (e) or Decrypt (other key)")
	val choice = scala.io.StdIn.readLine
	if (choice == "e") {
		encrypt
	} else {
		decrypt
	}
	
}

