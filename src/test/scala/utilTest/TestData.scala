package utilTest



/**
 *
 */

object TestData {
	final val SAMPLE_SIZE: Int = 10000
	final val SAMPLE_SIZE_LARGE: Int = 100000

	// Number of times to combine the t-digests
	final val NUM_MONOIDAL_ADDITIONS = 20

	// Kolmogorov Smirnov epsilon limit bound
	final val EPSILON_CDF: Double = 0.02 // value copied from isarn-sketches-spark tests
	final val EPSILON_SMP: Double = 0.05 // value (maxD) from isarn-sketches tests: https://github
	// .com/isarn/isarn-sketches/blob/develop/src/test/scala/org/isarnproject/sketches/TDigestTest.scala#L52-L56

	final val EPSILON_T: (Double, Double) = (EPSILON_CDF, EPSILON_SMP)

	//Simple identifier to keep track of which test is running
	//var TEST_ID: (Int, Char) = (0, 'a')

	// Max discrete parameter - the higher, the more discrete estimates are included in the discrete distribution
	// t-sketch estimates
	// TODO find out exactly algorithmically what it means
	final val MAX_DISCRETE = 500 // same value as placed in the isarn-sketches-spark test to make discrete
	// distribution t-sketches pass (otherwise not enough t-sketch discrete estimates are included so KSD can't
	// converge)

}