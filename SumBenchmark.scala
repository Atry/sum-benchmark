import org.scalameter.api._
import org.scalameter.picklers.Implicits._

object SumBenchmark extends Bench.LocalTime {
  
  val arrays = for {
    size <- Gen.exponential("size")(256, 2097152, 2)
  } yield Array.fill[Float](size)(math.random.toFloat)
  
  
  private def simpleReduceSum(array: Array[Float]): Float = {
    var i = 0
    var result = 0.0f
    while (i < array.length) {
      result += array(i)
      i += 1
    }
    result
  }
  
  measure method "simpleReduceSum" in {
    using (arrays) in { array =>
      simpleReduceSum(array)
    }
  }
  
  private def simdReduceSum4(array: Array[Float]): Float = {
    val tmp = new Array[Float](4)
    val vectorLength = array.length - (array.length % tmp.length)
    var i = 0
    while (i < vectorLength) {
      var j = 0
      while (j < tmp.length) {
        tmp(j) += array(i + j)
        j += 1
      }
      i += tmp.length
    }
    var result = tmp.sum
    while (i < array.length) {
      result += array(i)
      i += 1
    }
    result
  }
  
  measure method "simdReduceSum4" in {
    using (arrays) in { array =>
      simdReduceSum4(array)
    }
  }
  
  
  private def simdReduceSum8(array: Array[Float]): Float = {
    val tmp = new Array[Float](8)
    val vectorLength = array.length - (array.length % tmp.length)
    var i = 0
    while (i < vectorLength) {
      var j = 0
      while (j < tmp.length) {
        tmp(j) += array(i + j)
        j += 1
      }
      i += tmp.length
    }
    var result = tmp.sum
    while (i < array.length) {
      result += array(i)
      i += 1
    }
    result
  }
  
  measure method "simdReduceSum8" in {
    using (arrays) in { array =>
      simdReduceSum8(array)
    }
  }
  
  
  private def simdReduceSum16(array: Array[Float]): Float = {
    val tmp = new Array[Float](16)
    val vectorLength = array.length - (array.length % tmp.length)
    var i = 0
    while (i < vectorLength) {
      var j = 0
      while (j < tmp.length) {
        tmp(j) += array(i + j)
        j += 1
      }
      i += tmp.length
    }
    var result = tmp.sum
    while (i < array.length) {
      result += array(i)
      i += 1
    }
    result
  }

  measure method "simdReduceSum16" in {
    using (arrays) in { array =>
      simdReduceSum16(array)
    }
  }  

  performance of "ArrayOps" in {
    measure method "sum" in {
      using (arrays) in { array =>
        array.sum
      }
    }
  }

}

// vim: set ts=2 sw=2 et:
