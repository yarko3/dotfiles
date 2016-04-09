import os
import sys

spark_home = '/home/brh/spark-1.6.0-bin-hadoop2.6'

sys.path.insert(0, '/home/brh/spark-1.6.0-bin-hadoop2.6/python')
sys.path.insert(0, os.path.join(spark_home, 'python/lib/py4j-0.9-src.zip'))

execfile(os.path.join(spark_home, 'python/pyspark/shell.py'))
