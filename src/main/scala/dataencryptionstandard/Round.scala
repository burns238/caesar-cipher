package dataencryptionstandard

import scala.annotation.tailrec
import dataencryptionstandard.SubkeyFunctions._
import dataencryptionstandard.BitFunctions._
import dataencryptionstandard.Permutation._
import dataencryptionstandard.BitMappings._
import dataencryptionstandard.Sbox._

object Round {

	//Recursive function to run through all 16 rounds. The initial roundCount needs to be 1.
	@tailrec
	def round(leftBits: Vector[Char], rightBits: Vector[Char], initialSubKey: Vector[Char], count: Int): Vector[Char] = {
		count match {
			case 17 => leftBits ++ rightBits
			case c => {
				implicit val roundNumber = c
				val subKey = permutedChoice2(initialSubKey)	
				val nextRightBits = encryptBits(leftBits, rightBits, subKey)
				round(rightBits, nextRightBits, subKey, c+1)
			}
		}
	}

	private def encryptBits(leftBits: Vector[Char], rightBits: Vector[Char], subKey: Vector[Char])(implicit roundNumber: Int): Vector[Char] = {
		val xorWithSubKeyFunc = (bits: Vector[Char]) => xor(bits, subKey)

		val encrypted = (expand andThen xorWithSubKeyFunc andThen applySbox _ andThen roundPermutation)(rightBits)
		xor(encrypted, leftBits)
	}
	
}