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
    println("Enter number of iterations")
  val m  = scala.io.StdIn.readInt()
  var evaporation = 0.7
  
    val Filename= "/home/amara/SampleData"// data set
    
    /*val lines1 = Source.fromFile(Filename).getLines.toList
    println("new lines are")
   //val lines_  =  lines1.getLines.toList
   var edges_  =  lines1.map(_.split(" "))
   
   var newedges  =  edges_.map(x=>x.mkString(","))
   var finaledges  =  newedges.map(x  =>x.split(","))
   finaledges.foreach(println)*/
   
   
    val lines = Source.fromFile(Filename).getLines().toList
    val nodes = lines.map(_.toString()).filter(_.!=("\n")).filter(_.!=(" ")).filter(_.!=("\r")).toList.distinct
    //val nodes = lines.map(_.toString()).filter(_.!=(" ")).toList.distinct // Finding Nodes from Data set
   nodes.foreach(println)
   var nodes1=nodes.map(_(0))
   var nodes2=nodes.map(_(2))
   
   // println("Nodes are "+nodes.length)
   val TotalNoOfNodes = nodes.length
   println(" Total No Of Nodes are : " + TotalNoOfNodes)
   
   
   //val edges_temp  =  lines_.split(" ")
   
   //  val edges = lines.map(_.toString()).filter(_.!=("\n")).filter(_.!=(" ")).filter(_.!=("\r")).toArray   // Finding Nodes from Data set

 // val lines1 = Source.fromFile(Filename)
  val edges = List.fill(nodes.length)(1)
   // val edges = Array.fill(nodes.length)(1)
//print(" " + edges.foreach(println))
  //    println()
  //edges.foreach(println) 
  //val totalEdges = edges.length
  //println("Edges are: " + totalEdges)
   
   //var matrix = MutableList.fill(TotalNoOfNodes, TotalNoOfNodes)(0,0)
   var adjacencyMatrix = Array.ofDim[Int] (TotalNoOfNodes, TotalNoOfNodes)
   for (i <- 0 to edges.length-1 ){
    // for (j <- 0 to nodes2.length-1){
     adjacencyMatrix(i)(i) = 1
     //}
   }
    println("Adjacency Matrix is: ")
     for (i <- 0 to nodes.length-1){
         for (j <- 0 to nodes.length-1){
        print(" " + adjacencyMatrix(i)(j))
    }
     println()
     }
     
    var pheromoneMatrix = Array.ofDim[Double] (TotalNoOfNodes, TotalNoOfNodes)
     for (i <- 0 to nodes1.length-1){
       for (j <- 0 to nodes2.length-1){
        if( adjacencyMatrix(i)(j) == 1){
          pheromoneMatrix (i)(j) =pheromoneCalculate(lambda, abselon, 1)
         //pheromoneMatrix (i)(j) = lambda * (1 + abselon)
        }
        else{
          
          pheromoneMatrix (i)(j) =pheromoneCalculate(lambda, abselon, 0)
          //pheromoneMatrix (i)(j) = lambda * (0 + abselon)
        }
      
    }
     }
    //
    println("Pheromone Matrix")
    for (i <- 0 to nodes1.length-1){
     for (j <- 0 to nodes2.length-1){
        print("  " + pheromoneMatrix(i)(j))
     }  
      println()
    
    }
    //heuristic matrix
    var heuristicMatrix = Array.ofDim[Double](TotalNoOfNodes, TotalNoOfNodes)
    for (i <- 0 to nodes1.length-1){
     for (j <- 0 to nodes2.length-1){
      heuristicMatrix(i)(j) = gemma * commonNeighbours(adjacencyMatrix, i, j)
    }
    }
    //printing heuristic matrix
    
     for (i <- 0 to nodes1.length-1){
     for (j <- 0 to nodes2.length-1){
      print(" " +heuristicMatrix(i)(j))
     }
      println()
    
     }
     
    // calculating probability of ants
    
    for (k <- 1 to m){
       for (i <- 0 to nodes1.length-1){
         for (j <- 0 to nodes2.length-1){  
            if (adjacencyMatrix(i)(j)==1){
             pheromone = pheromoneMatrix(i)(j)
             //def ** (Alpha: Double) = Math.pow (pheromone,alfa)
              p = Math.pow (pheromone,alpha)
             println("Pheromone value")
             print(" "+ p)
             
           println()
       }
          if (adjacencyMatrix(i)(j) == 1){
            heuristic = heuristicMatrix(i)(j)
            //def ** (Beta: Double) = Math.pow(heuristic, beta)
             h = Math.pow(heuristic, beta)
            println("Heuristic value")
             print(" "+ h)
             println()
          } 
          
     }
       for (i <- 0 to nodes1.length-1){
          for (j <- 0 to nodes2.length-1){
           
             Sum = Sum + adjacencyMatrix(i)(j)
           
           
     }
         }
         println("Calculating Fitness Sum")
         println("Sum")
         print(" "+ Sum)
         println()
         
         Quality = C*1 / TotalNoOfNodes* Sum
         println("Quality")
         print (" " + Quality )
         println()
         
         for (l <- 1 to m){
           sum = sum + Quality
         }
         println("sum is")
         print(" " + sum)
         println()
      }
     // for (i <- 1 to m){
        prod = p * h
          println("product of pheromone and heuristic is")
          print(" " + prod)
          println()
          summ = summ + prod
           println("summation of pheromone and heuristic is")
           print(" " + summ)
           println()
     // }
      
      prob = prod / summ
         println("Probability is")
         print(" " + prob)
         println()
         
      
      if(k == 1){
        Prob = prob
        println("\nprobability is : " +prob)
      }
      if(prob > Prob){
        Prob = prob
        println("\nprob is : " +prob)
      
    }
    }
    println("\nHighest Probability is " + Prob)
      //prob=  evaporation + 7.6
        
    println()
     
    
   
    var iteration = Array.ofDim[Double](TotalNoOfNodes, TotalNoOfNodes)
     for (i <- 0 to nodes1.length-1){
       for (j <- 0 to nodes2.length-1){
        iteration(i)(j) = evaporation*pheromoneMatrix(i)(j) + 7.6
        //prob=evaporation+7.6
        print( " " + iteration(i)(j))
     }
      println()
    }
   readData(Filename) 
    
  }
  def pheromoneCalculate(lambda:Double,abselon:Double,link:Int):Double={
    lambda*(link+abselon)
  }
  
  def commonNeighbours(matrix:Array[Array[Int]], x: Int, y: Int ): Int = {
    var count = 0
    for (i <- 0 to matrix.length-1){
      if (matrix(x)(i) == 1 && matrix(y)(i) == 1){
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