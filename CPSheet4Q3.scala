import io.threadcso._
import scala.collection.mutable.Queue

object CPSheet4Q3{ 
    abstract class IntTree 
    case class Leaf(n: Int) extends IntTree 
    case class Branch(l: IntTree, r: IntTree) extends IntTree 

    class TreeSummer {

        def sumTree(tree: IntTree) = {
            val resultChan = OneOne[Int]
            run(summer(tree,resultChan) || component.console(resultChan))
        }

        private def summer(tree: IntTree, sendUp: Chan[Int]): PROC=proc{
            tree match {
                case Leaf(n) =>
                    sendUp!n
                case Branch(l,r) =>
                    val fromL,fromR = OneOne[Int]
                    run(summer(l,fromL) || summer(r,fromR) || adder(fromL,fromR,sendUp))
            }
        }

        private def adder(fromL: Chan[Int],fromR: Chan[Int],up: Chan[Int]):PROC=proc{
                    val lVal=fromL?()
                    fromL.close
                    val rVal=fromR?()
                    fromR.close
                    up!(lVal+rVal)
                    up.close
        }

    }

    def main(args:Array[String]):Unit= {
        val l1=Leaf(10)
        val l2=Leaf(2)
        val l3=Leaf(3)
        val l4=Leaf(1)
        val l5=Leaf(8)
        val l6=Leaf(5)
        val b1=Branch(l1,l2)
        val b2=Branch(l3,b1)
        val b3=Branch(l4,l5)
        val b4=Branch(b2,b3)
        val tree=Branch(b4,l6)

        println("Expected total: 29")
        print("Actual total: ")
        val summer = new TreeSummer
        summer.sumTree(tree)

    }

}