package dataencryptionstandard

object Round {

	def leftShift(subKey: Vector[Char], round: Int): Vector[Char] = {
		subKey.map(c => (c << round).toChar)
	}

	def permutedChoice2(leftKey: Vector[Char], rightKey: Vector[Char]): Vector[Char] = {
		???
	}
	
}