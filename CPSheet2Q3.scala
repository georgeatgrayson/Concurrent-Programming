import io.threadcso._
import scala.io
import scala.collection.mutable.Queue

object CPSheet2Q3{ 

    def buff[T](in: ?[T], out: ![T]) = proc{
        var queue = Queue[T]()
        def input=proc{
            while(true){
                queue.enqueue(in?())
            }
        }
        def output=proc{
            while(true){
                if(queue.isEmpty==false){
                    out!queue.dequeue()
                }
            }
        }
        run(input || output)
    }

    var inInt,outInt = OneOne[Int]

    def main(args:Array[String])= {
        //run(buff[Int](inInt,outInt))


        //exit()
    }

}