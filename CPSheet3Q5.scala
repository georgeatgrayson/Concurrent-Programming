
import io.threadcso._
import scala.util.Random

object CPSheet3Q5{ 
    type Row = Array[Boolean] 
    type Image = Array[Row]
    val numWorkers = 10
    val cutoffValue = 1000
    class smoothing{
    	private var rounds = 0
    	private var baseImage:Image=Array.fill(100,100)(false)
    	private var updatedImage:Image=Array.fill(100,100)(false)
    	val finalBarrier=new Barrier(2)
    	val roundBarrier=new Barrier(numWorkers+1)
    	val workerBarriers=Array.fill(numWorkers)(Barrier(1))
    	var w=0
        var xsize=0
        var ysize=0
        var changeOccured=false
    	for(w<-0 to numWorkers+1) workerBarriers(w)=new Barrier(numWorkers-w)


        def apply(image: Image)={
        	baseImage=image
        	xsize=baseImage(0).length
        	ysize=baseImage.length
        	changeOccured=false
        	(|| (for (i <- 0 until numWorkers) yield worker(i)) || controller)()
            

            finalBarrier.sync()
            baseImage
        }
        private def worker(me:Int)=proc{
        	var rowProgress=0
        	while(rounds< cutoffValue){
        		roundBarrier.sync()
        		rowProgress=0
        		while(rowProgress*numWorkers+me < ysize){
	        		val rownum=rowProgress*numWorkers+me
	        		val currentRow=baseImage(rownum)
	        		var newRow=new Row(xsize)
	        		var i=0
	        		var changeInRow=false
	        		for(i <- 0 to xsize-1){
	        			val shouldset = shouldSet(rownum,i)
	        			changeInRow = changeInRow || shouldset==baseImage(rownum)(i)
	        			baseImage(rownum)(i)=shouldset
	        		}
	        		for (i<-0 to me){
	        			workerBarriers(i).sync()
	        		}
	        		updatedImage(rownum)=newRow
	        		changeOccured = changeOccured || changeInRow
	        		workerBarriers(me).sync()
	        		rowProgress+=1
        		}

        	}
        }
        private def shouldSet(r:Int,c:Int):Boolean={
        	var x=0
        	var tot=1
        	if(baseImage(r)(c))x+=1
        	if(r!=0){
        		tot+=1
        		if(baseImage(r-1)(c))x+=1
        	}
        	if(c!=0){
        		tot+=1
        		if(baseImage(r)(c-1))x+=1
        	}
        	if(r!=0&&c!=0){
        		tot+=1
        		if(baseImage(r-1)(c-1))x+=1
        	}
        	if(r!=ysize-1&&c!=0){
        		tot+=1
        		if(baseImage(r+1)(c-1))x+=1
        	}
        	if(r!=0&&c!=xsize-1){
        		tot+=1
        		if(baseImage(r-1)(c+1))x+=1
        	}
        	if(r!=ysize-1&&c!=xsize-1){
        		tot+=1
        		if(baseImage(r+1)(c+1))x+=1
        	}
        	if(r!=ysize-1){
        		tot+=1
        		if(baseImage(r+1)(c))x+=1
        	}
        	if(c!=xsize-1){
        		tot+=1
        		if(baseImage(r)(c+1))x+=1
        	}
        	(2*x>tot)
        }

        private def controller=proc{
        	while(rounds< cutoffValue || !changeOccured){
        		var rowProgress=0
        		while(rowProgress< ysize/numWorkers){
	        		var i=0
	        		for (i<-0 to numWorkers){
	        			workerBarriers(i).sync()
	        		}
	        		rowProgress+=1
        		}
        		baseImage=updatedImage
        	}
        	finalBarrier.sync()
        }
    }

    def test()={
    	val s=new smoothing
    	val tests =1000
    	var i=0
    	for(i<-0 to tests){
    		val baseImage = Array.fill(100,100)(Random.nextBoolean())
    		s(baseImage)
    		print(".")
    	}
    }

    def main(args : Array[String])={
    	test()
    }
}