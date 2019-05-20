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
	def round(leftBits: Vector[Char], rightBits: Vector[Char], subKeys: Seq[Vector[Char]], count: Int): Vector[Char] = {
		count match {
			case 17 => leftBits ++ rightBits
			case c => {
				val nextRightBits = encryptBits(leftBits, rightBits, subKeys(c-1))
				round(rightBits, nextRightBits, subKeys, c+1)
			}
		}
	}

	private def encryptBits(leftBits: Vector[Char], rightBits: Vector[Char], subKey: Vector[Char]): Vector[Char] = {
		val expanded = expand(rightBits)
		val xoredWithSubKey = xor(expanded, subKey)
		val encrypted = (applySboxes _ andThen roundPermutation)(xoredWithSubKey)
		xor(encrypted, leftBits)
	}
	
}