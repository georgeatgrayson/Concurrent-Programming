import io.threadcso._
import scala.collection.mutable.Queue

object CPSheet2Q5{ 
    class Synchronisation {

        //val men=Queue[String]()
        //val women=Queue[String]()

        val inMen=ManyOne[String]
        val inWomen=ManyOne[String]
        val outMen=OneMany[String]
        val outWomen=OneMany[String]

        private def pairer() = proc{
            while(true){
                val m = inMen?()
                val w = inWomen?()
                outMen!(w)
                outWomen!(m)
            }
        }

        def manSync(me: String):String = {
            inMen!(me)
            outMen?()
        }

        def womanSync(me: String):String = {
            inWomen!(me)
            outWomen?()
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