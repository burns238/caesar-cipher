package dataencryptionstandard

import scala.annotation.tailrec
import dataencryptionstandard.SubkeyFunctions._
import dataencryptionstandard.BitFunctions._
import dataencryptionstandard.Permutation._
import dataencryptionstandard.BitMappings._

object Round {

	//Recursive function to run through all 16 rounds. The initial roundCount needs to be 1.
	@tailrec
	def round(leftBits: Vector[Char], rightBits: Vector[Char], initialSubKey: Vector[Char], count: Int): Vector[Char] = {
		count match {
			case 17 => leftBits ++ rightBits
			case c => {
				implicit val subKey = permutedChoice2(initialSubKey, c)
				val nextRightBits = encryptBits(leftBits, rightBits)
				round(rightBits, nextRightBits, subKey, c+1)
			}
		}
	}

	private def encryptBits(leftBits: Vector[Char], rightBits: Vector[Char])(implicit subKey: Vector[Char]): Vector[Char] = {
		val encryptedRightBits = (expand andThen xorWithSubKey andThen sbox _ andThen roundPermutatation)(rightBits)
		xor(encryptedRightBits, leftBits)
	}
	
}