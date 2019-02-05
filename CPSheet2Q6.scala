import io.threadcso._
import scala.collection.mutable.Queue
import scala.util.Random

object CPSheet2Q6{ 

    def f(x:Double,y:Double)={
        math.max(x,y)
    }

    class Ring(n:Int){ 
        require(n >=2)
        private val chan=Array.fill(n)(OneOne[Double])

        def apply(me:Int,x:Double):Double= { 
            val in=chan(me); val out=chan((me+1)%n) 
            if(me==0){ 
                out!x 
                val y=in?()
                out!y
                in?() 
                y
            } else{ 
                val z=in?() 
                out!f(z,x)
                val y=in?()
                out!y
                y
            } 
        }

    }

    var xs:Array[Double]=null
    var results:Array[Double]=null
    def thread(me:Int,r:Ring)=proc("Thread"+me){
        val x=Random.nextDouble(); xs(me)=x
        val result=r(me,x)
        results(me)=result
    }

    def runTest= {
        val n=1+Random.nextInt(20)
        val r = new Ring(n)
        xs= new Array[Double](n); results= new Array[Double](n)

        run(|| (for (i <- 0 until n) yield thread(i,r)))
            //Checkresults
        var fx=xs(0)
        var i=1
        while(i < n){
            fx=f(fx,xs(i))
            i+=1
        }
        i=0
        while(i < n){
            assert(results(i)==fx, "xs="+xs.mkString(",")+"\nresults="+results.mkString(","))
            i+=1
        }
        
    }
}