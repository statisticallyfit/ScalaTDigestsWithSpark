/**
 *
 */


import org.apache.spark.sql.SparkSession
import scala.Predef.String

object try2 {


	val spark = SparkSession.builder()
		.master("local[1]")
		.appName("SparkByExamples.com")
		.getOrCreate();
}
