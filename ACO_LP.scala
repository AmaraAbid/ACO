package acoProject
import scala.io.Source
import scala.collection.mutable.MutableList

object ACO_LP {
  var GList : List [(String, String)] = List()
  var Subject : List [String] = List()
  var Object : List [String] = List()
  def main(args: Array[String]){
    val lambda=0.01
     val abselon=0.01
     val alpha=0.8
     val beta=0.7
     val gemma = 0.01
     var pheromone=0.1
    var heuristic = 1.0
    var Sum = 0.0
    var C = 0.95
    var Quality = 0.0
    var sum = 0.0
    var prod = 1.0
    var summ = 0.0
    var h = 1.0
    var p = 1.0
    var Prob = 0.0
    var prob = 0.0
    var degree = 0.0
    println("Enter number of iterations")
    val iter  = scala.io.StdIn.readInt()
    println("Enter number of ants")
    val m  = scala.io.StdIn.readInt()
    var evaporationRate = 0.7
  
    val Filename= "/home/amara/SampleData"// data set
    val lines = Source.fromFile(Filename).getLines().toArray
    val nodes = lines.map(_.toString()).filter(_.!=("\n")).filter(_.!=(" ")).filter(_.!=("\r")).toArray.distinct
    var t=nodes.map(x=>x.split(" ")).map(x=>(x(0).toInt,x(1).toInt,x(2).toInt))
    nodes.foreach(println)
    var nodes1=nodes.map(_(0))
    var nodes2=nodes.map(_(2))
    var DisNodes = (nodes1 ++ nodes2).distinct
    DisNodes.foreach(println)
    
    val TotalNoOfNodes = DisNodes.length    //^^^^^^^^^^^^^^^^^^^
    println(" Total No Of Nodes are : " + TotalNoOfNodes)
    val edges = List.fill(DisNodes.length)(1)    //  Edges Vector
    
    //Adjacency matrix
    var adjacencyMatrix = Array.ofDim[Int] (TotalNoOfNodes, TotalNoOfNodes)
    for (i <- 0 to t.length-1 ){
      adjacencyMatrix(t(i)._1)(t(i)._2) = (t(i)._3)
    }
     //Pheromone Matrix
    var pheromoneMatrix = Array.ofDim[Double] (  TotalNoOfNodes, TotalNoOfNodes  )
    println("Pheromone Matrix is ")
    for (i <- 0 to DisNodes.length-1)  {  //^^^^^^^^^^^^^^
      for (j <- 0 to DisNodes.length-1)  {  //^^^^^^^^^^^^^^^^^^^^
       pheromoneMatrix (i)(j)  =  pheromoneCalculate  (  lambda,  abselon,  adjacencyMatrix(i)(j)  )
       print(" " +pheromoneMatrix(i)(j))
    }
      println()
    }
    
    //Heuristic matrix
    var heuristicMatrix = Array.ofDim[Double](TotalNoOfNodes, TotalNoOfNodes)
    println("Heuristic Matrix is ")
    for (i <- 0 to DisNodes.length-1){ //^^^^^^^^^^^^
     for (j <- 0 to DisNodes.length-1){ //^^^^^^^^^^^
      heuristicMatrix(i)(j) = gemma * commonNeighbours(adjacencyMatrix, i, j)
      print(" " +heuristicMatrix(i)(j))
     }
     println()
    }
    
    // calculating probability of ants
    for(i <- 0 to iter-1){
     for (k <- 0 to m-1){
      var Path = Array.ofDim[Double](  m  , TotalNoOfNodes)
      var start  =  scala.util.Random.nextInt  (  DisNodes.length  )  //^^^^^^^^^^^
      Path(k)(0)  =  start
      for (i <- 1 to DisNodes.length){
        var adjacentTo  =  adjacencyMatrix(start)//.filter(_==1)
        var nextnode:Int = -1
        var highProb:Double = -1.0
        
        for(  adj  <-  adjacentTo  )  {
          //pheromone calcualtion
          pheromone = pheromoneMatrix(start)(adj)
          p = Math.pow(pheromone, alpha)
           println("Pheromone value")
           print(" "+ p)
           println()
           //heuristic calculation
           heuristic = heuristicMatrix(start)(adj)
           h = Math.pow(heuristic, beta)
            println("Heuristic value")
             print("hueristic"+ heuristic+"   beta "+beta+"    HeuristicCalc "+heuristic)
             println()
             //calculate probability
           prod = p * h
           println("product of pheromone and heuristic is")
           print(" " + prod)
           println()
           summ = summ + prod
           println("summation of pheromone and heuristic is")
           print(" " + summ)
           println()
      
           prob = prod / summ
           println("Probability is")
           print(" " + prob)
           println()
           
         //  Path(m)(i)  =  nextnode
          //start = nextnode 
           /*highProb = prob
           if(prob > highProb){
              highProb = prob
              println("\nprob is : " +highProb)
           }*/
        }
      }
      //calculate Fitness Sum
      for (i <- 0 to DisNodes.length-1){
       for (j <- 0 to DisNodes.length-1){
        degree = degree + adjacencyMatrix(i)(j)
       }
      println("Degree of nodes is " + degree)
      Quality = C * 1 / TotalNoOfNodes* degree
      println("Quality")
      print (" " + Quality )
      println()         
      degree = 0
      }    
     }
     //updating pheromone values
      var pUpdation = Array.ofDim[Double](TotalNoOfNodes, TotalNoOfNodes)
       for (i <- 0 to nodes1.length-1){
        for (j <- 0 to nodes2.length-1){
          pUpdation(i)(j) = evaporationRate*pheromoneMatrix(i)(j) + Quality
          print( " " + pUpdation(i)(j))
        }
        println()
        }
    }
}  
  def pheromoneCalculate(lambda:Double,abselon:Double,link:Int):Double={
    lambda*(link+abselon)
  } 
  def commonNeighbours(matrix:Array[Array[Int]], x: Int, y: Int ): Int = {
    var count = 0
    for (i <- 0 to matrix.length-1){
      if (  matrix(x)(i) == 1 && matrix(y)(i) == 1  )  {
        count = count +1
      }
    }
    count
  }
  def readData(fname: String)
  {
  val lines = Source.fromFile(fname).getLines.toList
  for (i <- 0 to lines.length-1){
   var tokens = lines(i).split(" ")
   GList = ( tokens(0) , tokens(1) ) :: GList
  }
  println("GraphList: " + GList)
 }
}
