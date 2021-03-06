package workspace_snippets

import org.isarnproject.sketches.TDigest
import util.GeneralUtil
import util.distributionExtensions.distributions._
import util.distributionExtensions.instances.AllInstances._
import util.distributionExtensions.syntax._

import scala.reflect.runtime.universe._
import utilTest.TestData._
import utilTest.TestTools.StatTools._
import utilTest.TestTools.SpecsTools._
import smile.stat.distribution.GammaDistribution
import utilTest.TestTools.GeneralTools._
/**
 *
 */
object try_IsarnSketch_ChangingGammas_MANY extends App {

	val bs = (10 to 100 by 10).map(_.toDouble).toList
	// Li of modes that you want the distributions to have - this way, can calculate alphas out of betas and modes,
	// and also specify how far the dists are from each other.
	val targetModes = (1 to 50 by 1).map(_.toDouble).toList
	//List(10, 20, 30, 40, 50, 60, 70, 80, 90, 100).map(_.toDouble)
	// since mode = (alpha - 1)/beta for gamma
	val asBsModes: List[(Double, Double, Double)] = (for(m <- targetModes; b <- bs) yield((m * b + 1, b, m)))
	//.filter {case (a, b, m) => a <= bs.max} // so that the alpha doesn't get hideously large


	val gammasModes: Seq[(Double, GammaDist)] = asBsModes.map { case (a, b, m) => (m, GammaDist(a, b) ) }
	println(s"unsorted modes = $gammasModes")
	println(s"just modes = ${gammasModes.unzip._1}")

	// sort by mode
	//val gammasIncreasingModes: Seq[(Double, GammaDist)] = gammasModes.sortBy{ case (mode, gammaDist) => mode }
	//println(s"SORTEDmodes = $gammasIncreasingModes")

	// Now cherry pick from the gamma increasing modes list to get increasing amounts of the gammas with the higher
	// modes, to weigh the combinations towards a dist that has a higher mode
	// 1) First split the gammas into groups by same modes
	val groups: Map[Double, Seq[(Double, GammaDist)]] = gammasModes.groupBy(_._1) // group by same modes
	println(s"groups = $groups")
	// Sort by mode (increasing):
	val groupsSorted: Seq[Seq[(Double, GammaDist)]] = groups.toSeq.sortBy(_._1).unzip._2 // get just
	// mode-gammas
	println(s" sorted GROUPED modes = $groupsSorted")

	// TODO: new steps
	// 2) pick head of each sorted groups (the smaller alpha-beta pair, the flatter the distribution, otherwise gets
	// too narrow)
	val firstsGroupsSorted: Seq[(Double, GammaDist)] = groupsSorted.map(_.head)
	// 3) get the last 1/4 of the list. Repeat the gammas like 1, 2, 4, 8,.. (geometric by 2)) (use repeatTail
	// function)
	val conedGroups: Seq[(Double, GammaDist)] = repeatTail(firstsGroupsSorted, numReps = 10, numIncr = 3) //
	// take last
	// 10, repeat as 1, 3, 9, 27, 81, ...
	val gammasMovingRight: Seq[GammaDist] = conedGroups.unzip._2

	println(s"coned groups = $conedGroups")
	// 4) then add those in the t-sketch


	// NOTE: OLD stuff here
	/*// 2) Create list of increasing numbers to weight the list by (to choose num of gammas according to the nums in
	// this list)
	val numPicks: List[Int] = (1 to gammasGroupsSorted.length).toList
	// 3)  Pair the gammas and modes with the amount to choose from each group of modes
	val numsGroups: List[(Int, Seq[(Double, GammaDist)])] = numPicks.zip(gammasGroupsSorted)
	println(s"picks paired with groups: $numsGroups")
	// 4) Take from each gamma-mode group as many as the required (cone-style, more of ones which higher mode and
	// less of the ones with small mode, to encourage gammas to shift right)
	val gammaModesMovingRight: List[(Double, GammaDist)] = numsGroups.flatMap{ case (n, lst) => lst.reverse.take(n)
		.reverse }
	// HELP why does flatMap here work but .map.flatten have error "no implicits found for asTraversable"?

	println(s"gammas moving right = ${gammaModesMovingRight}")

	val gammasMovingRight: Seq[GammaDist] = gammaModesMovingRight.unzip._2 // get just the gamma dists*/



	/*
	// TODO: little test here, adding gammas manually, using modes that are moving right, to test the moving right
	//  hypothesis can actually work, to find out what is wrong with the above code: (try increasing distance
	//  between dists)
	val green: GammaDist = GammaDist(9, 33) // left-placed gamma
	val red: GammaDist = GammaDist(47, 49)
	val purple: GammaDist = GammaDist(92, 26)
	val orange: GammaDist = GammaDist(168, 12)
	val small: GammaDist = GammaDist(501, 10)

	val tdGreen = TDigest.sketch(green.sample(SAMPLE_SIZE))
	val tdRed = TDigest.sketch(red.sample(SAMPLE_SIZE))
	val tdPurple = TDigest.sketch(purple.sample(SAMPLE_SIZE))
	val tdOrange = TDigest.sketch(orange.sample(SAMPLE_SIZE))
	val tdSmall = TDigest.sketch(small.sample(SAMPLE_SIZE))

	val smallShift: Seq[TDigest] = List(tdRed, tdPurple, tdOrange, tdSmall)
		.scanLeft(TDigest.empty())((ltd, rtd) => TDigest.combine(ltd, rtd))
		.drop(1)


	val fitter: TDigest => GammaDistribution = td => GammaDistribution.fit(
		Array.fill[Double](SAMPLE_SIZE)(td.samplePDF)
	)
	val makeFits: Seq[TDigest] => Seq[GammaDist] = seqTDs => seqTDs
		.map(td => fitter(td))
		.map(g => GammaDist(g.k, g.theta))

	val makeModes: Seq[GammaDist] => Seq[Double] = seqGs => seqGs.map(g => calcMode(g))

	val makeCdfDiffs: Seq[GammaDist] => Seq[Double] = seqGs => seqGs.sliding(2, 1).toList
		.map { case List(g1, g2) => cdfSignTest(g1, g2) }

	val fits = makeFits(smallShift)
	/*val modes = fits.map(g => calcMode(g))
	val cdfDiffs = fits.sliding(2, 1).toList.map{ case List(g1, g2) => cdfSignTest(g1, g2) }*/


	println(s"\n\n\nCOMBINING manually separated dists by combining via tdigest: " )
	println(s"all modes = ${makeModes(fits)}")
	println(s"all fits = $fits")
	println(s"all cdf sign tests ${makeCdfDiffs(fits)}")

	// TODO: test 2 - making t-combines out of consecutive pairs, maybe tn the mode doesn't get so skewed each time?
	val pairwiseTDigests: Seq[TDigest] = List(tdGreen, tdRed, tdPurple, tdOrange, tdSmall)
		.sliding(2, 1).toList
		.map{ case List(ltd, rtd) => TDigest.combine(ltd, rtd) }

	val fitsList: Seq[GammaDist] = makeFits(pairwiseTDigests)

	println(s"\n\nCOMBINING manually separated PAIRWISE dists: ")
	println(s"all fits = ${fitsList}")
	println(s"all modes = ${makeModes(fitsList)}")
	println(s"all cdf diffs = ${makeCdfDiffs(fitsList)}")*/


	// TODO - test 3 - what happens if you don't add the gmas in order of increasing mode? (as opposed to increasing
	//  mode?)

	// CONCLUSION: doesn't work -- resulting gammas are still wonky and skewed not as expected

	// ----------------------------------------------------------------------

	// NOTE: now here the NUM_MONOIDAL_ADDITIONS is replaced by shiftData.length
	val shiftData: Seq[Array[Double]] = gammasMovingRight.map(gdist => gdist.sample(SAMPLE_SIZE_TEST))
	val firstTD: TDigest = TDigest.sketch(shiftData.head, maxDiscrete = MAX_DISCRETE)

	// Creating the sketches and combining them:
	val shiftedSketch: Seq[TDigest] = shiftData.tail
		.map((distSmp: Array[Double]) => TDigest.sketch(distSmp, maxDiscrete = MAX_DISCRETE))
		.scanLeft(firstTD)((ltd: TDigest, rtd: TDigest) => TDigest.combine(ltd, rtd, maxDiscrete = MAX_DISCRETE))
		.drop(1) // TODO look at algebird factory why erikerlandson drops 1 here


	// see how convergence is at the last update (after combining the sketches from ALL the distributions)
	val conceptDriftData = Array.fill[Double](SAMPLE_SIZE_TEST){shiftedSketch.last.samplePDF}


	val fit: GammaDistribution = GammaDistribution.fit(conceptDriftData)
	val distShifted: GammaDist = GammaDist(fit.k, fit.theta)
	val distFirst: GammaDist = gammasMovingRight.head
	val distLast: GammaDist = gammasMovingRight.last

	/*distShifted.getNumericalMean should beBetween(distFirst.getNumericalMean, distLast
		.getNumericalMean)

	// right-skew dist should be on the left of the combined dist (between the right and left-skewed dists)
	cdfSignTest(distFirst, distShifted) should beLessThan(0.0)
	// combined dist should be on the right of the left-tailed dist (between the right and left-skewed dists)
	cdfSignTest(distShifted, distLast) should beLessThan(0.0)*/

	// TODO shape seems confused with scale param here (from smile or from tdigest estimate?)

	val (a1, a2, a3, b1, b2, b3) = (distFirst.shape, distShifted.shape, distLast.shape, distFirst.scale, distShifted
		.scale, distLast.scale)

	println(s"all alphas, betas, modes = $asBsModes")

	println(s"alphas, betas: ${((a1, b1), (a2, b2), (a3, b3))}")
	println(s"means: ${distLast.getNumericalMean} <? ${distShifted.getNumericalMean} <? " +
		s"${distFirst.getNumericalMean}")
	println(s"modes = ${(calcMode(distFirst), calcMode(distShifted), calcMode(distLast))}")

	println(s"cdf sign test = ${cdfSignTest(distFirst, distShifted)}")
	println(s"cdf sign test = ${cdfSignTest(distShifted, distLast)}")
}
