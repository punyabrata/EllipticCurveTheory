import scala.actors.Actor
import scala.actors.Actor._

case class operands(start:Int, end: Int, length : Int){
  
}

class EllipticCurve(N:Int,k:Int) extends Actor {	
	val noOfActors = 6
	
	//the act method
	def act() {	  
		var count = 1
		var start = 1
		var end = Math.ceil(N.toDouble/noOfActors).toInt;
		var factor = end;
		
		//dividing the job equally to the number of actors
		while(count <= noOfActors){
			if(count == noOfActors)
				end = N;
			count = count +1
			val operand = new operands(start,end,k)
			start = end + 1;
			end = end + factor;
			val w = new work
			w.workers ! operand		  
		}
		
		//receiving the answers from the actors
		count = 1
		var ans = List[Int]()
		while(count <= noOfActors){
			count = count + 1
			receive{
				case l:List[Int]=>
				ans :::= l
			}
		}		

		//Printing the first number of each of such sequences
		println(ans)
	}

}

class work{
	def workers: Actor = {
		val worker = actor{
			react{
				case operand:operands =>			
					var sum = 0.0
					var sq = 0.0
					val N = operand.end
					val k = operand.length
					val st = operand.start

					//For debugging purpose
					//var msg = ""
					var points = List[Int]()
				  
					//Finding the said sequence 
					for (i:Int <- st to N){
						sum = 0.0
						for(j <- i to (i+k-1)){
							sum += j.toDouble*j.toDouble;
						}
						sq = Math.sqrt(sum)
						if(!((sq%1)>0)){
							points ::= i
							//Debugging information
							//msg = "num = "+i+" sum = "+sum+" sq = "+sq
					    }
					}
				sender ! points
			}
		}
		worker
	}
}

object Project1_Standalone {
	def main(args: Array[String]){
		val Eli = new EllipticCurve(args(0).toInt,args(1).toInt)
		Eli.start
	}
}
