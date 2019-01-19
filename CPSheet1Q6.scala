
import io.threadcso._
import scala.io
import util.Random.nextInt

object CPSheet1Q6{ 

	def main(args:Array[String])= {
		var i=0;
		while(i< args.length){
			println(args(i))
			Sort(args(i).toInt) 
		}
			exit()
	}


	private def Sort(n: Int){
		var a: Array[Int] = Seq.fill(n)(util.Random.nextInt).toArray

		var startTime=System.nanoTime()

		var x=0

		def arrayitems(out: ![Int])=proc{
			var i=0;
			while(i < n){
				out!a(i)
				i+=1
			}
		}

		def channels = new Array[Chan[Int]](n)

		var chanIn=OneOne[Int]
		var chanOut=OneOne[Int]

		var system=arrayitems(chanOut)

		chanIn=chanOut
		chanOut=OneOne[Int]
		var j=0
		for (j <- 0 to n){
			def pr(in: ?[Int], out: ![Int])=proc{ 
				var count=0
				var value =0
				var cval =0
				value=in?()
				while(true){
					cval=in?()
					if(cval>value){
						out!(value)
						value=cval
					}else{
						out!(cval)
					}
					count+=1
					if(count==n-1){
						out!(value)
					}
				}
			}
			def fin(in: ?[Int])=proc{ 
				var count=0
				while(true){
					//println(in?())
					count+=1
					if (count==n) {
						println((System.nanoTime-startTime)/1000000000.0+"s")
						exit()
					}
				}
			}
			if(j==n){
				system = system || fin(chanIn)
			}else{
				system = system || pr(chanIn,chanOut)
			}

			chanIn=chanOut
			chanOut=OneOne[Int]
		}
		run(system)
	}
}
