package dataencryptionstandard

import scala.annotation.tailrec
import dataencryptionstandard.BitMappings._

object Permutation {
	
	def permutate(bits: Vector[Char], permutation: Vector[Int]): Vector[Char] = {
		permutation.map(v => bits.apply(v-1))
	}

	val expand = (bits: Vector[Char]) => permutate(bits, Expansion)
	val roundPermutation = (bits: Vector[Char]) => permutate(bits, RoundPermutation)
	val initialPermutation = (bits: Vector[Char]) => permutate(bits, InitialPermutation)
	val inversePermutation = (bits: Vector[Char]) => permutate(bits, FinalPermutation)
	val permutedChoice1 = (bits: Vector[Char]) => permutate(bits, PermutedChoice1)
	val permutedChoice2 = (bits: Vector[Char]) => permutate(bits, PermutedChoice2)
}