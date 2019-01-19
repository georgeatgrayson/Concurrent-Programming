import io.threadcso._
import scala.io
import util.Random.nextInt

object CPSheet1Q7{ 

	var numWorkers=200		//Value doesn't matter, is overwritten

	def main(args:Array[String])= {
		numWorkers=args(0).toInt
		var x=args(1).toInt
		var y=args(2).toInt		//These are only for testing
		var z=args(3).toInt

		val random = new java.security.SecureRandom   
		var A: Array[Array[Double]] = Array.fill(y, x) { (random.nextDouble()-0.5)*100 }
		var B: Array[Array[Double]] = Array.fill(z, y) { (random.nextDouble()-0.5)*100 }
		var i=0;
		startTime=System.nanoTime()
		Multiply(A,B) 
		exit()
	}

	private type Task = (Int,Int)
	private val toWorkers=OneMany[Task]
	private type Result = (Double,Int,Int)
	private val toController=ManyOne[Result]
	private var MatrixA:Array[Array[Double]]=null
	private var MatrixB:Array[Array[Double]]=null
	private var MatrixC:Array[Array[Double]]=null

	private var startTime=0L

	private def Multiply(A: Array[Array[Double]],B: Array[Array[Double]]){
		var l=B.length
		var d=A(0).length
		MatrixC=Array.ofDim[Double](l,d)
		MatrixA=A
		MatrixB=B

		var system=controller || starter

		for(i<-0 until numWorkers){
			var w = worker
			system = system || w
		}

		run(system)

		exit()
	}


	private def worker=proc("worker"){
		var n = MatrixA.length
		while(true){
			val (x,y)=toWorkers?()
			var total:Double=0
			for(i <- 0 until n){
				total=total+MatrixA(i)(y)*MatrixB(x)(i)
			}
			toController!(total,x,y)
		}
	}

	private def starter=proc{
		var l=MatrixB.length
		var d=MatrixA(0).length
		for(i<-0 until l*d){
			toWorkers!(i % l,i/l)
		}
	}

	private def controller=proc{
		val size=MatrixA(0).length*MatrixB.length
		var returned=0

		while(returned< size){
			val (tot,x,y) = toController?()
			MatrixC(x)(y)=tot.toInt
			returned+=1
		}
		println((System.nanoTime()-startTime)/1000000+"ms")
		//println(MatrixC.deep.mkString("\n"))
		exit()
	}
}