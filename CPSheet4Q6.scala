import io.threadcso._
import scala.util.Random
import scala.collection.mutable.ListBuffer

object CPSheet4Q6{ 

  class BoundedBuffer[T]{

    val n = 100
    val space=CountingSemaphore(n)
    val values=CountingSemaphore(0)
    val buffer = new ListBuffer[T]

    def insert(item: T)={
      space.down
      values.up
      buffer+=(item)
    }

    def apply(index: Int):T={
      buffer.apply(index)
    }

    def clear()={
      buffer.clear()
    }

    def length=buffer.length

    def remove(index: Int):T={
      values.down
      space.up
      buffer.remove(index)
    }

    def update(index: Int, newElem: T)={
      buffer.update(index,newElem)
    }

  }

  def main(args : Array[String]){
    val b = new BoundedBuffer[Int]
    def adder=proc{
      var i=0
      while(i<1000){
        b.insert(i)
        i+=1
      }
    }
    def remover=proc{
      var i=0
      Thread.sleep(2000)
      while(i<1001){
        println(b.remove(0))
        i+=1
      }
    }
    run(adder || remover)

  }


}