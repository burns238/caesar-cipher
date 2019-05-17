package dataencryptionstandard

case class RoundInterface(
	leftBits: Vector[Char], 
	rightBits: Vector[Char], 
	leftKey: Vector[Char], 
	rightKey: Vector[Char]
)