package shiftencryption

object Caesar extends App {

	val DefaultKey = 3

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
		println("Write text to encrypt")
		val plainText = scala.io.StdIn.readLine
		println(CaesarEncryption.encryptText(plainText, readKey))
	} else {
		println("Write text to decrypt")
		val plainText = scala.io.StdIn.readLine()
		println(CaesarEncryption.decryptText(plainText, readKey))
	}
	
}

