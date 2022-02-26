WARNING:

To run the cdf-spline example, need to set java-11 as the JDK or else will get this error:

Exception in thread "main" java.lang.UnsupportedClassVersionError: com/manyangled/snowball/analysis/interpolation/MonotonicSplineInterpolator has been compiled by a more recent version of the Java Runtime (class file version 55.0), this version of the Java Runtime only recognizes class file versions up to 52.0
	at java.lang.ClassLoader.defineClass1(Native Method)
	at java.lang.ClassLoader.defineClass(ClassLoader.java:756)
	at java.security.SecureClassLoader.defineClass(SecureClassLoader.java:142)
	at java.net.URLClassLoader.defineClass(URLClassLoader.java:468)
	at java.net.URLClassLoader.access$100(URLClassLoader.java:74)
	at java.net.URLClassLoader$1.run(URLClassLoader.java:369)
	at java.net.URLClassLoader$1.run(URLClassLoader.java:363)
	at java.security.AccessController.doPrivileged(Native Method)
	at java.net.URLClassLoader.findClass(URLClassLoader.java:362)
	at java.lang.ClassLoader.loadClass(ClassLoader.java:418)
	at sun.misc.Launcher$AppClassLoader.loadClass(Launcher.java:355)
	at java.lang.ClassLoader.loadClass(ClassLoader.java:351)
	at CDFSpline$.generateSpliningComparisons(CDFSpline.scala:44)
	at util.graph.CDFSplineRunner$.delayedEndpoint$util.graph.CDFSplineRunner$1(CDFSpline.scala:131)
	at util.graph.CDFSplineRunner$delayedInit$body.apply(CDFSpline.scala:127)
	at scala.Function0.apply$mcV$sp(Function0.scala:39)
	at scala.Function0.apply$mcV$sp$(Function0.scala:39)
	at scala.runtime.AbstractFunction0.apply$mcV$sp(AbstractFunction0.scala:17)
	at scala.App.$anonfun$main$1$adapted(App.scala:80)
	at scala.collection.immutable.List.foreach(List.scala:431)
	at scala.App.main(App.scala:80)
	at scala.App.main$(App.scala:78)
	at util.graph.CDFSplineRunner$.main(CDFSpline.scala:127)
	at util.graph.CDFSplineRunner.main(CDFSpline.scala)



SOURCE: https://www.baeldung.com/java-lang-unsupportedclassversion
* Java 8 == version 52
* Java 11 == version 55