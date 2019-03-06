import io.threadcso._
import scala.collection.mutable.Queue

object CPSheet4Q7{ 
    class Synchronisation {

        var manName = ""
        var womanName = ""
        val manLock = MutexSemaphore()
        val womanLock = MutexSemaphore()
        val manLock1 = MutexSemaphore()
        val womanLock1 = MutexSemaphore()
        val manLock2 = MutexSemaphore()
        val womanLock2 = MutexSemaphore()
        manLock2.down
        womanLock2.down

        def manSync(me: String):String = {
            manLock.down
            womanLock1.down
            manName=me
            manLock2.up
            womanLock2.down
            val name= womanName
            manLock.up
            manLock1.up
            name
        }

        def womanSync(me: String):String = {
            womanLock.down
            manLock1.down
            womanName=me
            womanLock2.up
            manLock2.down
            val name= manName
            womanLock.up
            womanLock1.up
            name
        }

    }

    def main(args:Array[String])= {
        val synchro = new Synchronisation
        run(proc{println(synchro.manSync("Dave")+" Dave")}||
            proc{println(synchro.manSync("Steve")+" Steve")}||
            proc{println(synchro.womanSync("Emma")+" Emma")}||
            proc{println(synchro.womanSync("Jess")+" Jess")})
    }

}