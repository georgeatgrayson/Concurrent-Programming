import io.threadcso._
import scala.collection.mutable.Queue

object CPSheet2Q4{ 

    val epsilon=0.1
    val numWorkers=100
    val ins=OneMany[(Double,Double)]
    val outs=ManyOne[Option[Double]]
    val intervals=Queue[(Double,Double)]()
    var numIntervals=0
    var estimate=0.0
    def estimate(f: Double => Double, a:Double, b:Double){

            def mainThread() = proc{
                var system = taskThread || addingThread
                for(i<-0 until numWorkers){
                    var w = estimateWorker(ins,outs)
                    system = system || w
                }
                numIntervals=1
                run(system)
                intervals.enqueue((a,b))
            }

            def taskThread = proc{
                while(numIntervals>0) if(intervals.isEmpty==false) ins!intervals.dequeue
                println(estimate)
                exit()
            }

            def addingThread = proc{
                while(true){
                    val out=outs?()
                    out match {
                      case Some(b) =>
                        estimate+=b
                        numIntervals-=1
                      case default =>
                        numIntervals-=1
                    }
                }
            }

            def estimateWorker(rangeIn: ?[(Double,Double)], out: ![Option[Double]]) = proc{
                while(true){
                    val (ca,cb)=rangeIn?()
                    val mid=(ca+cb)/2.0
                    val fa=f(ca); val fb=f(cb); val fmid=f(mid)
                    val lArea=(fa+fmid)*(mid-a)/2; val rArea = (fmid+fb)*(b-mid)/2
                    val area=(fa+fb)*(b-a)/2
                    if(Math.abs(lArea+rArea-area) < epsilon) out!Some(area)
                    else {
                        numIntervals+=2
                        intervals.enqueue((ca,mid))
                        intervals.enqueue((mid,cb))
                        out!None
                    }
                }
            }

    }


}