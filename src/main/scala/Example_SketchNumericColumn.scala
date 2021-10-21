import org.apache.spark.sql.expressions.UserDefinedFunction
import org.apache.spark.sql.{DataFrame, Row, SparkSession}
import org.isarnproject.sketches.java.TDigest
import org.isarnproject.sketches.spark.tdigest.TDigestAggregator

import scala.util.Random.{nextGaussian, nextInt}

/**
 *
 */
object Example_SketchNumericColumn extends App {

	val spark: SparkSession = SparkSession.builder()
		.master("local[1]").appName("SparkByExamples.com")
		.getOrCreate()

	import spark.sqlContext.implicits._ // NOTE: need these implicits for the $"_1" in the udf arg to work


	// Sketch a numeric column
	val data: DataFrame = spark.createDataFrame(Vector.fill(1000) {
		(nextInt(10), nextGaussian)
	})
	Console.println(s"data = $data")

	val udf: UserDefinedFunction = TDigestAggregator.udf[Double](compression = 0.2, maxDiscrete = 25)
	Console.println(s"udf = $udf")

	val agg: Row = data.agg(udf($"_1"), udf($"_2")).first
	Console.println(s"agg = $agg")

	val (td1, td2) = (
		agg.getAs[TDigest](0),
		agg.getAs[TDigest](1)
	)
	Console.println(s"td1 = $td1")
	Console.println(s"td2 = $td2")

	Console.println(s"td1.cdf(2) = ${td1.cdf(2)}")
	Console.println(s"td2.cdf(2) = ${td2.cdf(2)}")

	Console.println(s"td1.samplePMF = ${td1.samplePMF}")
	Console.println(s"td2.samplePDF = ${td2.samplePDF}")
}
