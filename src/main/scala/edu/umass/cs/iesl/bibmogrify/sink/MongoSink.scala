package edu.umass.cs.iesl.bibmogrify.sink

import edu.umass.cs.iesl.bibmogrify.NamedPlugin
import edu.umass.cs.iesl.bibmogrify.pipeline.{TransformerMetadata, Sink}
import edu.umass.cs.iesl.scalacommons.NonemptyString
import com.mongodb.{Mongo, BasicDBObject, BasicDBList}
import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.bibmogrify.model.StructuredCitation
import java.io.{PrintWriter,File}
import edu.umass.cs.iesl.bibmogrify.model.RichStructuredCitation._


object Mongo2TextExporter{
  def main(args:Array[String]) ={
    val pw = new PrintWriter(new File("paper-mapping.txt"))
    val papers = FUSEMongoSink.paperCollection.find//.hint("$natural")
    //val papers = FUSEMongoSink.paperCollection.find.hint("$natural")
    //println("Mongo conn: "+FUSEMongoSink.mongoConn+" MongoDB:"+FUSEMongoSink.mongoDB+" col: "+FUSEMongoSink.paperCollection)
    while(papers.hasNext){
      val paper = papers.next
      val mentions = paper.get("ms").asInstanceOf[BasicDBList]
      if(mentions.size>1){
        var i = 0
        while(i<mentions.size){
          var j = i+1
          while(j<mentions.size){
            pw.println(mentions.get(i).toString+" "+mentions.get(j).toString)
            j+=1
          }
          i+=1
        }
      }
    }
    pw.flush()
    pw.close()
  }
}

object FUSEMongoSink extends Sink[(NonemptyString, StructuredCitation)] with NamedPlugin with Logging {
	var citationCount: Long = 0L
	var timer               = 0L
	//hard coded for now. TODO: figure out how to make these command line arguments
	lazy val mongoConn       = new Mongo("localhost", 27017)
	lazy val mongoDB         = mongoConn.getDB("bibmogrify")
	lazy val paperCollection = mongoDB.getCollection("papers")
	//paperCollection.ensureIndex(new BasicDBObject("_id",1))

	//size
	//	val writer: BufferedWriter = new BufferedWriter(new OutputStreamWriter(scala.Console.out))
	//** use monadic IO
	var lock: AnyRef = new Object()

	def put(b: (NonemptyString, StructuredCitation)) {
		//		lock.synchronized{
		if (timer == 0L) timer = System.currentTimeMillis
		val entityId = b._1.toString
		val mentionId = b._2.primaryId.toString
		//val dbo = JSON.parse(c).asInstanceOf[DBObject]
		//println("entityId: "+entityId+" mentionId: "+mentionId)
		/*
			  val payload = new BasicDBObject("$push", new BasicDBObject("ms",mentionId))
			  payload.put("$inc",new BasicDBObject("sz",1))
			  payload.put("_id",entityId)
			  //println("  PAYLOAD: "+payload.toString)
			  paperCollection.update(select,payload,true,false)
			  */
		//ugh, I have to do this check because mongo modifiers do not work with upserts.
		val select = new BasicDBObject("_id", entityId)
		if (paperCollection.findOne(select) == null) {
			//println("INSERT: "+entityId)
			val dbl = new BasicDBList;
			dbl.add(mentionId)
			val payload = new BasicDBObject("_id", entityId)
			payload.put("ms", dbl)
			payload.put("sz", 1)
			paperCollection.insert(payload)
			//paperCollection.insert((new BasicDBObject("_id",entityId)).put("ms",dbl).put("sz",1))
		}
		else {
			//println("UPDATE:"+entityId)
			//val payload = new BasicDBObject("_id",entityId)
			//payload.put("$push",new BasicDBObject("ms",mentionId))
			//payload.put("$inc",new BasicDBObject("sz",1))
			val payload = new BasicDBObject("$push", new BasicDBObject("ms", mentionId))
			payload.put("$inc", new BasicDBObject("sz", 1))
			//println("  PAYLOAD: "+payload.toString)
			paperCollection.update(select, payload)
		}
		citationCount += 1L
		if (citationCount % 1000L == 0L) print(".")
		if (citationCount % 20000L == 0L) {
			val elapsed = (System.currentTimeMillis - timer) / 1000L
			val citesPerSec: Int = if (elapsed == 0L) -1 else (citationCount.toLong / elapsed).toInt
			println(" processed " + citationCount + " citations in " + (elapsed) + " sec. Citations per sec: " + citesPerSec)
		}
		//		}
	}

	def putMetadata(m: Option[TransformerMetadata]) {
		lock.synchronized {

			                  paperCollection.ensureIndex(new BasicDBObject("ms", 1)) //mentions
			                  paperCollection.ensureIndex(new BasicDBObject("sz", 1))
			                  //throw new Exception("Error, not yet implemented.")
			                  //m.map(r => writer.write(r.toString))
		                  }
	}

	val name = "mongo"

	def close() {
		lock.synchronized {
			                  mongoConn.close
		                  }
	}
}



/*
class FileSink(filename:String) extends Sink[String] with NamedPlugin {
  val writer: BufferedWriter = new BufferedWriter(new FileWriter(filename))

  def put(c: String) {
    writer.write(c)
  }

  val name = "console"

}
*/
