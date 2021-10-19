/**
 *
 */
import org.isarnproject.sketches.java.TDigest
import org.isarnproject.sketches.spark.tdigest._

import scala.util.Random._
import org.apache.spark.sql.SparkSession

object tryout {

	val spark:SparkSession = SparkSession.builder()
		.master("local[1]").appName("SparkByExamples.com")
		.getOrCreate()


	// Sketch a numeric column
	val data = spark.createDataFrame(Vector.fill(1000){
		(nextInt(10), nextGaussian)
	})

	val udf = TDigestAggregator.udf[Double](compression = 0.2, maxDiscrete = 25)

	val agg = data.agg(udf($"_1"), udf($"_2")).first

	val (td1, td2) = (
		agg.getAs[TDigest](0),
		agg.getAs[TDigest](1)
	)

	println(td1.cdf(2))
	println(td2.cdf(2))

	println(td1.samplePMF)
	println(td2.samplePDF)
}
