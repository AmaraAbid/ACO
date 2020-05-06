package acoProject
import scala.io.Source
import scala.collection.mutable.MutableList

object ACO_LP {
  var Subject : List [String] = List()
  var Object : List [String] = List()
  var Link : List [String] = List()
  var Entities : List [String] = List()         //#  Why there was a need to mantain a list of entities and then Distinct Entities in separate variables ?
  var DisEntities : List [String] = List()
  var AProd : Array [Double] = Array()          //#  Avoid arrays as much as possible. Prefer Lists until its necessary
  var AProb : Array [Double] = Array()
  var Path : Array [Int] = Array()
  var Weight : Array [Int] = Array()
  var IProb: Array[Double] = Array()
  var cUpdated : Array[Int] = Array()
  var iComp : Array[Int] = Array()
  var AIndex : Array[Int] = Array()
  var nDegree : Array[Double] = Array()
  var nQuality : Array[Double] = Array()
  val lambda=0.01
  val abselon=0.01
  val gemma = 0.01
  var pheromone=0.1
  var heuristic = 1.0
  var a =0
  var b = 0
  var p = 1.0
  var h = 1.0
  val alpha=0.8
  var beta = 0.7
  var prod = 1.0
  var prob = 0.0
  var sum =0.0
  var C = 0.95
  var nextNode:Int = -1
  var WeightIs:Int = -1
  var degree = 0.0
  var Quality = 0.0
  var eRate = 0.7
  var p1 = 0
  var p2 = 0
  
  def main (args:Array[String]){
    
    println("Enter number of iterations")
    val iter  = scala.io.StdIn.readInt()
    
    println("Enter number of ants")
    val m  = scala.io.StdIn.readInt()
    
    val Filename= "/home/amara/newData"// data set
    for(line <-Source.fromFile(Filename).getLines ){
      println(line)
    }
    
    var lines = Source.fromFile(Filename).getLines().toArray 
    for(i<- 0 to lines.length-1){
      var s = lines(i).split(",")
      Subject = s(0) :: Subject
      Object = s(2) :: Object
      Link = s(1) :: Link
      Entities = (Subject ::: Object)
      DisEntities = (Subject ::: Object).distinct
      
    }
    
    println("...........Subjects are...........")
    Subject.foreach(println)
    
    println("...........Objects are...........")
    Object.foreach(println)
    
    println("...........Links are...........")
    Link.foreach(println)
    
    println("...........Entities are...........")
    Entities.foreach(println)
    
    println("...........Entities are...........")
    DisEntities.foreach(println)
    
    val TotalNoOfNodes = DisEntities.length    
    println(" Total No Of Nodes are : " + TotalNoOfNodes)
    
    val TotalNoOfNodes1 = Entities.length    
    println(" Total No Of Nodes are : " + TotalNoOfNodes1)
    
    var adjacencyMatrix = Array.ofDim[Int] (TotalNoOfNodes1, TotalNoOfNodes1)
    println("Adjacency Matrix is " )
    
     for (i <- 0 to lines.length-1 ){
         adjacencyMatrix(Subject(i).toInt)(Object(i).toInt) = (Link(i).toInt)
         adjacencyMatrix(Object(i).toInt)(Subject(i).toInt) = (Link(i).toInt)
      
     }
    
    for (i <- 1 to DisEntities.length+1){
      for (j <- 1 to DisEntities.length+1){
          print(" " +adjacencyMatrix(i)(j))
      }
      
      println()
    
   }
    
       var pheromoneMatrix = Array.ofDim[Double] (  TotalNoOfNodes1, TotalNoOfNodes1  )
       println("Pheromone Matrix is " )
   for (i <- 1 to DisEntities.length+1){  
      for (j <- 1 to DisEntities.length+1){ 
          if(adjacencyMatrix(i)(j) != 0){
             pheromoneMatrix (i)(j)  =  pheromoneCalculate  (  lambda,  abselon,  1  ) 
          }
        
        else{
             pheromoneMatrix (i)(j)  =  pheromoneCalculate  (  lambda,  abselon,  0  )
        }
          print(" " +pheromoneMatrix(i)(j))
      }
      println
  }
    
        var heuristicMatrix = Array.ofDim[Double](TotalNoOfNodes1, TotalNoOfNodes1)
        println("Heuristic Matrix is " )
  
   for (i <- 1 to DisEntities.length+1)  {  
      for (j <- 1 to DisEntities.length+1){
            var count = 0
           for(v <- 1 to DisEntities.length+1 ){
               if(adjacencyMatrix(i)(v) != 0 && adjacencyMatrix(j)(v) != 0){
                   count = count +1
                }
           }
        
            heuristicMatrix(i)(j) = gemma * count
            print(" " +heuristicMatrix(i)(j))
       
      }
      println()
  }
    
        var c : Array[Int] = Array()
    
    
    for(i <- 0 to iter-1){
        for (k <- 0 to m-1){
       
               var start  =  scala.util.Random.nextInt  (  DisEntities.length +1 )
               println("Starting node "+start)
            
               Path = Array.fill(DisEntities.length)(0)
               Weight = Array.fill(Path.length)(0)
       
         for (i <- 1 to DisEntities.length){
       
               degree = 0.0
               Quality = 0.0
               Path = Path.updated(i-1, start)
               println("Path array is")
               for(a<- Path){
                  println(a)
                 }
         
                var adjacentTo  =  adjacencyMatrix(start).filter(_!= 0)
                AProd  = Array.fill(adjacentTo.length)(0)
                AProb  = Array.fill(adjacentTo.length)(0)
                println("adjacent value"+adjacentTo.length)
                   for(ai <-adjacentTo ){
                      println(ai)     
                    }

                  var arr : Array [Int] = Array()  
             for(i <- 0 to adjacentTo.length-1){
          
                var highProb:Double = -1.0
                var a = adjacencyMatrix(start).zipWithIndex
                var d = a.filter(x => (x._1)!=0)
                c = d.map(x=> (x._2))
                var e =  c.zipWithIndex
         
          println("Nodesof Adjacent with index is is ")
           for(pi<- e){
            println(pi)
          }
           
          println("only Nodes are ")
          for(pi<- c){
            println(pi)
          }
         
        /////////////pheromone calculation//////////////////
            pheromone = pheromoneMatrix(start)(c(i))
            p = Math.pow(pheromone, alpha)
           
       //////////////heuristic calculation///////////////////////
           heuristic = heuristicMatrix(start)(c(i))
           h = Math.pow(heuristic, beta) 
           prod = p * h
           sum = sum + prod
           AProd = AProd.updated(i, prod)
           
        
      }
       
           for(i<- 0 to AProd.length-1){
              prob = AProd(i)/sum
              AProb = AProb.updated(i, prob)
            }
                println("all probs are")
                    for(a <- AProb){
                       println(a)
                     }
       
       var maxProb = AProb.max
       println("Maximun Probability is "+maxProb)
       
       var index = AProb.indexOf(maxProb)
       println("indexes of max probability is "+ index)
       
       nextNode = c(index)
       println("Next node is "+ nextNode)
       
       println("weight of max probability is ")
       WeightIs = adjacentTo(index)
   
       var comp = 0
     
      for(j<-0 to Path.length-1){
        
            for(x<-0 to Path.length-1){
              
                if(nextNode == Path(x)){
              
            comp = nextNode
            println("compare found")
            
            var cIndex = c.indexOf(comp)
            var pIndex = AProb.indexOf(comp)
            iComp = c.patch(cIndex, Nil,  1) 
            println("compared elements" + comp)
            println("Remaining c's are")
            
            for(a <- iComp){
              println(a)
             }
               if(c.length == 1){
                println("PATH IS")
               }
              
              else{
                    
                    var IProb = AProb.patch(AProb.indexOf(maxProb), Nil, 1)
                    println("Remaining probs are")
                    var IIProb = IProb.zipWithIndex
                    for(a <- IIProb){
                        println(a)
                       }
             
                   AIndex = adjacentTo.patch(cIndex, Nil, 1)
                   println("Remaining adjacents are")
                   for(a <- AIndex){
                      println(a)
                    }
             
                    var maxIProb = IProb.max
                    println("Maximun of Remaining Probability is "+maxIProb)
            
                    var Iindex = IProb.indexOf(maxIProb)
                    println("indexes of max probability is "+ Iindex)
            
                    nextNode = iComp(Iindex)
                    println("Next Remaining node is "+ nextNode)
            
                    WeightIs = AIndex(Iindex)
                    println("Next Remaining WeightIs is "+ WeightIs)
                    
                    AProb =  IProb.clone()
                    
                    c = iComp.clone()
                    adjacentTo =  AIndex.clone()
           
           
           }
            println( )
    }           
    
    }    
   }
       
         Weight = Weight.updated(i-1, WeightIs)
       
         if(c.length != 0){
            start = nextNode
           }
     
       degree = Weight.foldLeft(0)(_ + _)
       println("degree of path is "+ degree)
       
          
  }
         ///////////////Calculating Fitness Sum/////////////////
          
               Quality = C * (1 * degree) / TotalNoOfNodes
               println("Quality of path is "+ Quality)
          
               nQuality = nQuality ++ Array(Quality)
               println("Quality Array is ")
                 for(a<- nQuality){
                    println(a)
                  }
  }
      
        
        /////////// pheromone updation /////////////
  
       for (i <-  0 to Path.length){
         
           for (j <-  0 to Path.length){
          
               for (k <-  j+1 to Path.length-1 ){
        
                   if(k == j+1 ){
                       var p1 = Path(j)
                       var p2 = Path(k)
                       var evp = eRate * pheromoneMatrix(p1)(p2) + Quality
                       pheromoneMatrix = pheromoneMatrix.updated(p1 ,pheromoneMatrix(p1).updated(p2, evp))
       
                   }
          
               }
                  print(" " +pheromoneMatrix(i)(j))
        }
                  println()
      }  
                  pheromoneMatrix =  pheromoneMatrix.clone()
  }  
  }
  
  def pheromoneCalculate(lambda:Double,abselon:Double,link:Int):Double={
    lambda*(link+abselon)
  }
}
