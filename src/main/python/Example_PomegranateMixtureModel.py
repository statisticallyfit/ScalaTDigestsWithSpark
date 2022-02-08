from random import gauss, randint


from isarnproject.sketches.spark.tdigest import *
#from org.apache.spark.sql import SparkSession
from pyspark.sql import SparkSession


# import org.apache.spark.sql.SparkSession

from scipy.stats import gamma
import numpy as np
import matplotlib.pyplot as plt
#%matplotlib inline
import seaborn;
seaborn.set_style('whitegrid')

from pomegranate import *

'''
a = 4.5
b = 2.1

# x = np.linspace(gamma.ppf(0.01, a), gamma.ppf(0.99, a), 100)


greenDist = gamma(9, 33)

x = np.linspace(greenDist.ppf(0.01), gamma.ppf(0.99), num=100)

fig, ax = plt.subplots(1, 1)
ax.plot(x, greenDist.pdf(x), 'k-', lw=2, label="green dist")
'''

# ####

spark: SparkSession = SparkSession.builder.appName("SparkByExample").getOrCreate()

data = spark.createDataFrame([[randint(1,5), gauss(0,1)] for x in range(5000)]).toDF("g","x")
grp = data.groupBy("g").agg(tdigestDoubleUDF("x").alias("tdigests"))
grp.show()

'''4
model = GeneralMixtureModel([NormalDistribution(4, 1), NormalDistribution(7, 1)])
model2 = GeneralMixtureModel([NormalDistribution(5, 1), ExponentialDistribution(0.3)])

x = numpy.arange(0, 10.01, 0.05)

plt.figure(figsize=(14, 3))
plt.subplot(121)
plt.title("~Norm(4, 1) + ~Norm(7, 1)", fontsize=14)
plt.ylabel("Probability Density", fontsize=14)
plt.fill_between(x, 0, model.probability(x))
plt.ylim(0, 0.25)

plt.subplot(122)
plt.title("~Norm(5, 1) + ~Exp(0.3)", fontsize=14)
plt.ylabel("Probability Density", fontsize=14)
plt.fill_between(x, 0, model2.probability(x))
plt.ylim(0, 0.25)
plt.show()
'''