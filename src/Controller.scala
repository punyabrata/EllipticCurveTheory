import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.remote.RemoteActor._
import scala.actors.remote.Node

class Boss(Nstr : String, kstr : String) extends Actor {
  val machinesToUse = Array("00","02")		//Select the machines here. Eg. to add lin114-04, add "04"
  val noOfActors = 4						//Select the number of Actors on each machine
  val N = Nstr.toInt
  val k = kstr.toInt
  var port = 0
  var msg:String = ""
  var Mcount = 1
  var Mstart = 1
  var Mend = Math.ceil(N.toDouble/machinesToUse.length).toInt;
  var Mfactor = Mend;
  def act(){
    println("Controller Started")
    for(i:Int <- 0 to machinesToUse.length-1){
	
    	//Select the Actor 
    	port = 9000+machinesToUse(i).toInt
		var remoteActor = select(Node("lin114-"+machinesToUse(i)+".cise.ufl.edu", port), Symbol("remoteActor"+machinesToUse(i)))
		
		//Debug
		//println("lin114-"+machinesToUse(i)+".cise.ufl.edu" + ",," + port + ",," + "remoteActor"+machinesToUse(i))
		
		//Divide Work
		if(i == machinesToUse.length)
		    Mend = N;
		  
		msg = Mstart + "," + Mend + "," + k + "," +noOfActors
		Mstart = Mend + 1;
		Mend = Mend + Mfactor;
		
		//Send the Message
		remoteActor ! msg
	}
	
	
    //Receive the Replies
    var combList = List[Int]()
	for(i:Int <- 0 to machinesToUse.length-1){
	    receive{
		  case l:List[Int]=>
		    combList :::= l
		}
	}
    combList.sortWith(_<_).foreach(println)
  }
}

object Controller {
 def main(args: Array[String]){
  val B = new Boss(args(0),args(1))
  B.start
}
}