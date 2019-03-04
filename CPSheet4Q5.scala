import io.threadcso._
import scala.util.Random

object CPSheet4Q5{ 

  class threeDivSync{

    var mutex=MutexSemaphore()
    var syncMutex=MutexSemaphore()
    var totalIds=0

    def enter(id: Int)={
      mutex.down
      syncMutex.down
      assert(totalIds % 3 == 0)
      totalIds+=id
      if(totalIds % 3 == 0) mutex.up
      syncMutex.up
    }

    def exit(id: Int)={
      totalIds-=id
      if(totalIds % 3 == 0) mutex.up
    }

  }

  def main(args : Array[String]){
    val tds = new threeDivSync

    def accessor(me: Int) = proc{
      while(true){
        Thread.sleep(scala.util.Random.nextInt(1000))
        tds.enter(me)
        println(me+": Accessing")
        Thread.sleep(scala.util.Random.nextInt(1000))
        tds.exit(me)
        println(me+": Exiting")
      }
    }

    (|| (for (i <- 0 until 20) yield accessor(i)))()
    println; exit
  }


}