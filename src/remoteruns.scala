import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.remote.RemoteActor._
import scala.actors.remote.Node


class remoteActor extends Actor {
  //The Act method
	  def act() {
	  println("remoteActor started")
	  
	  //Register on the Network
	  val host = java.net.InetAddress.getLocalHost.getHostName
	  var hostPart = host.split("-")(1)
	  val port = hostPart.slice(0,2)
	  println("Port : "+(9000 + (port.toInt))+" name : "+"remoteActor"+port);
	  alive(9000 + (port.toInt))
	  register(Symbol("remoteActor"+port),self)

	  //Receive the Message
	  receive{
	    case "stop" => 
	      println("says stop")
	      exit()
	    
	    case inpString : String =>
	      	println("command received")
	      	println(inpString)
			var input = inpString.split(",")
			var start = input(0).toInt
			val N = input(1).toInt
			val k = input(2).toInt
			val noOfActors = input(3).toInt
	      	
			//Devide the work among Actors and spawn them
			var count = 1
			var factor = Math.ceil((N - start).toDouble/noOfActors).toInt;
			var end = start + factor;
			while(count <= noOfActors){
			  if(count == noOfActors)
			    end = N;
			  count = count +1
			  val operand = List(start,end,k)
			  start = end + 1;
			  end = end + factor;
			  val w = new work
			  w.workers ! operand		  
			}
			
			//Receive the result
			count = 1
			var ans = List[Int]()
			while(count <= noOfActors){
			  count = count + 1
			  receive{
			    case l:List[Int]=>
			     ans :::=l
			  }
			}
			
			//Send the combined results back to the Controller
			sender ! ans
		}
	}
	
}


//Worker Actors
class work{
  	def workers: Actor = {
	  val worker = actor{
	    react{
	      	case operand:List[Int] =>
	      	  println(operand)    //Printed here for the demo
	      	  
	      	  //Core task
	      	  var sum = 0.0
			  var sq = 0.0
			  val st = operand(0)
			  val N = operand(1)
			  val k = operand(2)
			  var points = List[Int]()
			  for (i:Int <- st to N){
			    sum = 0.0
			    for(j <- i to (i+k-1)){
			      sum += j.toDouble*j.toDouble;
			    }
			    sq = Math.sqrt(sum)
			    if(!((sq%1)>0)){
			      points ::= i
			    }
		   }
	    //Sending the result back to RemoteActor
	    sender ! points
	    }
	  }
	//This function returns a worker Actor
	worker
	}
}

//main
object remoteruns  {
 def main(args: Array[String]){
  val RA = new remoteActor
  RA.start
  }
}
