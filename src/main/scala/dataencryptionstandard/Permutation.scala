package dataencryptionstandard

import scala.annotation.tailrec

object Permutation {
	
	def permutate(bits: Vector[Char], permutation: Vector[Int]): Vector[Char] = {
		permutation.map(v => bits.apply(v-1))
	}

	val expand = (bits: Vector[Char]) => permutate(bits, BitMappings.Expansion)
	val roundPermutation = (bits: Vector[Char]) => permutate(bits, BitMappings.Permutation)
	val initialPermutation = (bits: Vector[Char]) => permutate(bits, BitMappings.InitialPermutation)
	val inversePermutation = (bits: Vector[Char]) => permutate(bits, BitMappings.FinalPermutation)
}