package acoProject
import org.apache.hadoop.conf.Configuration
import scala.io.Source
import org.apache.hadoop.fs.FileSystem
import org.apache.hadoop.fs.Path
import scala.util.Random
import scala.math.pow
import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.graphx.lib
import org.apache.spark.graphx.GraphLoader
import org.scalatest.Entry
import org.apache.spark.rdd.RDD
import org.semanticweb.HermiT.tableau.TupleTableFullIndex.EntryManager

object ACO {
  
    val Sconf = new SparkConf().setAppName("Ant Colnoy Optimization_LP").setMaster("local[*]")// creates a local spark session
    val sc = new SparkContext(Sconf) // creating spark context
    val conf = new Configuration()
    conf.addResource("/usr/local/hadoop/etc/hadoop/core-site.xml")
    conf.addResource("/usr/local/hadoop/etc/hadoop/hdfs-site.xml");
    conf.set("fs.defaultFS","hdfs://localhost:54310")
  val path=new Path("hdfs://localhost:54310/user/SampleData")
  val fileSystem = FileSystem.get(conf)
  val stream=fileSystem.open(path)
   var Triplets : List [ ( String , String , String ) ]    =    List ( )
   var Subjects  :  List[String]  =  List()
    //  List of Distinct Objects
    var Objects  :  List[String]  =  List()
    //  List of Distinct Links
    var Links  :  List[String]  =  List()
    //List of Distinct Entities 
    var DisEntity:List[String]  =  List() 
  
  
  def main(args:Array[String]){
    def readlines=Stream.cons(stream.readLine, Stream.continually(stream.readLine))
 
   var v= readlines.takeWhile(_ != null).toList
   v.foreach(println) 
  
  for  (  i  <-  0  to  v.length-1  ) 
      {
    
        var tokens  =  v(i).split(" ")
          Triplets  =  (  tokens(0)  ,  tokens(1) , tokens(2)  )  ::    Triplets
          
          //Distinct Subjects
        var  s  =  Subjects.find(  _  ==  tokens(0) )
        if  (s.getOrElse("null")  ==  "null"  )
          Subjects  =  tokens(0)  ::  Subjects
        
        //Distinct Objects
        var  o  =  Objects.find(  _  ==  tokens(1) )
        if  (o.getOrElse("null")  ==  "null"  )
          Objects  =  tokens(1)  ::  Objects
          
          //Distinct Links
          var  l  =  Links.find(  _  ==  tokens(2) )
        if  (l.getOrElse("null")  ==  "null"  )
          Links  =  tokens(2)  ::  Links
          
           DisEntity = Subjects
        for (i <- 0 to Objects.length-1)
        {
          var e = DisEntity.find( _ == Objects(i))
          if(e.getOrElse("null") == "null" )
            DisEntity = Objects(i) :: DisEntity
      }
      }
    println("-----------------  SUBJECTS  -------------")
        Subjects.foreach(println)
        println("-----------------  OBJECTS  -------------")
        Objects.foreach(println)
        println("-----------------  LINKS  -------------")
        Links.foreach(println)
        println("-----------------  LIST  -------------")
        Triplets.foreach(println)
        println("-----------------  ENTITIES  -------------")
        DisEntity.foreach(println)
   
        var zwi= DisEntity.zipWithIndex
       
        //verticers list
        var zip: List[(Long, String)]=zwi.map(x=>(x._2.toLong,x._1))
        zip.foreach(println) 
        
        //Edges List
        val edges  =  Triplets.map  (  x  =>  Edge(x._1.toLong,  x._2.toLong,  x._3)  )
        edges.foreach(println)
   
   // RDD's of vertices and edges
        val vRDD: RDD[(VertexId,String)] = sc.parallelize(zip)
        val eRDD: RDD[Edge[String]] = sc.parallelize(edges)
        val nowhere = "nowhere"
        
         println("vRDD is")
        vRDD.collect().foreach(println)
   
        
        val graph1 = Graph(vRDD, eRDD, nowhere) 
        
        println("Vertices are")
        graph1.vertices.collect.foreach(println)
        println("Edges are")
        graph1.edges.collect.foreach(println)
        
        
        //var AdjacencyMatrix = List[(String, String, String)]()
         //val AdjacencyMatrix: Matrix = Matrices.dense(Subjects.length, Objects.length, Array(1.0, 0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0,1.0, 0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0,1.0, 0.0, 0.0, 1.0, 1.0, 1.0 ))
    /* for(i <- 0 to Triplets.length-1)
    {
        //AdjacencyMatrix=AdjacencyMatrix:::List((Subjects(i),Objects(i),Links(i)))
        AdjacencyMatrix.foreach(println)
    }*/
        
        val graph2 = GraphLoader.edgeListFile(sc, "/home/amara/SampleData" , true).partitionBy(PartitionStrategy.RandomVertexCut)
        //val graph2 = GraphLoader.edgeListFile(sc, "/home/amara/SampleData" , true).partitionBy(new CustomPartitioner(2))
        
       
        
        val graph3 = graph2.subgraph(vpred = (vid, attr) => attr == 9)
        
        println("-------------------------- Graph3-----------------------")
        var l=graph2.edges.partitions
        
   l.foreach(println)
        //graph3.edges.collect().foreach(println)
        graph2.edges.mapPartitionsWithIndex{
      (indx,iter)=>
        {
          var v=iter.toList
          v.iterator
        }
    }
       //graph2.edges.m
       /* val triCounts = graph2.triangleCount().vertices
        triCounts.foreach(println)
              // Join the triangle counts with the usernames
        val users = sc.textFile("data/graphx/users.txt").map { line =>
        val fields = line.split(",")
        (fields(0).toLong, fields(1))
}*/
        
        val mapped =   eRDD.mapPartitionsWithIndex{
      
                        (index, iterator) => {
                           println("Called in Partition -> " + index)
                           val myList = iterator.toList
                          myList.foreach(println)
                           myList.map(x => x + " - " + index).iterator
                           
                          
                          //var AdjacencyMatrix : List[(String, String, String)] = AdjacencyMatrix.map(x=> (x._1,x._2,x._3))
                         //var AdjacencyMatrix: Iterator[(String, String, String)] = AdjacencyMatrix.map(x=> (x._1,x._2,x._3))
             
                 //myList.toIterator
                   
                        }
                     }
        
    mapped.collect()   
    
    }
  }