import io.threadcso._
import scala.util.Random

object CPSheet3Q3{ 

    /** Calculate prefix sums of an array a of size n in poly-log n (parallel)
  *  steps.  Based on Andrews Section 3.5.1. */
    class PrefixSums(n: Int, a: Array[Int]){
      require(n == a.size)

      /** Shared array, in which sums are calculated. */
      private val sum = new Array[Int](n) 

      /** Barrier synchronisation object. */
      private val barrier = new Barrier(n)
      private val summerBarriers = Array.fill(n)(new Barrier(2))

      /** Channels on which values are sent, indexed by receiver's identity. */
      private val toSummers =
        Array.fill(n)(0)
        //Array.fill(n)(N2NBuf[Int](size = 1, readers = 1, writers = n-1))

      /** An individual thread.  summer(me) sets sum[me] equal to sum(a[0..me]). */
      private def summer(me: Int) = proc("summer"+me){
        // Invariant: gap = 2^r and s = sum a(me-gap .. me]
        // (with fictious values a(i) = 0 for i < 0).  r is the round number.
        var r = 0; var gap = 1; var s = a(me)
        while(gap < n){
          if(me+gap < n){
            toSummers(me+gap)=s; summerBarriers(me+gap).sync() // pass my value up the line
          } 
          if(gap <= me){                // receive from me-gap,
            summerBarriers(me).sync()
            val inc = toSummers(me) // inc = sum a(me-2*gap .. me-gap]
            s = s + inc              // s = sum a(me-2*gap .. me]
          }
          r += 1; gap += gap           // s = sum a(me-gap .. me]
          barrier.sync()
        }
        sum(me) = s
      }

      /** Calculate the prefix sums. */
      def apply(): Array[Int] = {
        (|| (for (i <- 0 until n) yield summer(i)))()
        sum
      }
    }


    val reps = 10000

      /** Do a single test. */
    def doTest = {
        // Pick random n and array
        val n = 1+Random.nextInt(20)
        val a = Array.fill(n)(Random.nextInt(100))
        // Calculate prefix sums sequentially
        val mySum = new Array[Int](n)
        var s = 0
        for(i <- 0 until n){ s += a(i); mySum(i) = s }
        // Calculate them concurrently
        val sum = new PrefixSums(n, a)()
        // Compare
        assert(sum.sameElements(mySum),
               "a = "+a.mkString(", ")+"\nsum = "+sum.mkString(", ")+
                 "\nmySum = "+mySum.mkString(", "))
    }

    def main(args : Array[String]){
        for(r <- 0 until reps){ doTest; if(r%100 == 0) print(".") }
        println; exit
    }


}