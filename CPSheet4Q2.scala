import io.threadcso._
import scala.collection.mutable.Queue

object CPSheet2Q5{ 
    class Synchronisation {

        private def pairer() = proc{
            while(true){
                val m = inMen?()
                val w = inWomen?()
                outMen!(w)
                outWomen!(m)
            }
        }

        def manSync(me: String):String = synchronized{
            if(womenQueue.isEmpty()){
                menQueue.enqueue(me)
                while(womenQueue.isEmpty()) wait()
                womenQueue.dequeue()
            }
        }

        def womanSync(me: String):String = synchronized{
            if(menQueue.isEmpty()){
                womenQueue.enqueue(me)
                while(menQueue.isEmpty()) wait()
                menQueue.dequeue()
            }
        }

        run(pairer)

    }

    def main(args:Array[String])= {
        val synchro = new Synchronisation
        run(proc{println(synchro.manSync("Dave")=="Emma")}||
            proc{println(synchro.manSync("Steve")=="Jess")}||
            proc{println(synchro.womanSync("Emma")=="Dave")}||
            proc{println(synchro.womanSync("Jess")=="Steve")})
    }

}