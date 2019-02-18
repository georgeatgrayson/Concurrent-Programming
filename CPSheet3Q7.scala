import io.threadcso._
import scala.util.Random

object CPSheet3Q7{ 

  class threeDivSync{

    var totalIds=0

    def enter(id: Int)=synchronized{
      while(totalIds % 3 != 0) wait()
      assert(totalIds % 3 == 0)
      totalIds+=id
    }

    def exit(id: Int)=synchronized{
      totalIds-=id
      notifyAll()
    }

  }

  def main(args : Array[String]){
    val tds = new threeDivSync

    def accessor(me: Int) = proc{
      while(true){
        Thread.sleep(scala.util.Random.nextInt(100))
        tds.enter(me)
        println(me+": Accessing")
        Thread.sleep(scala.util.Random.nextInt(100))
        tds.exit(me)
      }
    }

    (|| (for (i <- 0 until 20) yield accessor(i)))()
    println; exit
  }


}