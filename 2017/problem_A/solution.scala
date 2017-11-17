/**
This solves the first problem from the qualifacation round for the 2017 google code jam
https://code.google.com/codejam/contest/dashboard?c=3264486
*/

import scala.io.Source

object PancakeFlipper{
	
	/**
		Entry point, runs the full program
	*/
	def main(args:Array[String]){

		val fileContents: Array[String] = readFile(args(0)):		
		val lineCount: Int = contentsArray(0).toInt;
	
		var lineNumber: Int = 1;
		for(problemLine <- contentsArray.slice(1, contentsArray.length)){
			try{
				val numberOfFlips: String = solveProblem(problemLine);
				println("Case #" + lineNumber + ": " +numberOfFlips);
			}catch{
				//Should figure out a better way to this than general exception. Other exceptions can cause this block to be triggered incorrectly. Need ot learn more scala
				case ex: Exception => {
					println("Case #" + lineNumber + ": " + "IMPOSSIBLE");
				}
			}
			lineNumber+=1;
		}
	}

	/**
	Read the content of a file and return an array of the lines
	*/
	def readFile(fileName: String): Array[String] = {
		val fileContents = Source.fromFile(fileName).mkString
		val contentsArray: Array[String] = fileContents.split("\n")
		return contentsArray;
	}

	/**
	Solves each problem line. Walk left to right and flip everytime we see
	a pancake that needs flipping. Maybe not the best algorithm
	*/
	def solveProblem(problemLine: String): String = {

		val problemArray: Array[String] = problemLine.split(" ");
		var pancakes: String = problemArray(0);
		val flipperLength: Int = problemArray(1).toInt;
		
		var numberOfFlips = 0
		while(!solved(pancakes)){
			var transformed = flipNextPancakes(pancakes, flipperLength);
			numberOfFlips+=1;			
			pancakes = transformed;
		}
		return numberOfFlips.toString
	}
	
	/**
	Finds and flips the next pancakes in sequence if able. Otherwise
	throws an exception. Should throw a more specialized exception
	*/
	def flipNextPancakes(pancakes: String, flipperLength: Int): String = {
		val firstUnFlipped: Int = pancakes.indexOf('-');
		if(firstUnFlipped + flipperLength > pancakes.length){
			throw new Exception;
		}
		else{
			var updatedPancakes: String = pancakes;
			val lastPancake: Int = firstUnFlipped+flipperLength - 1;
			for(i <- firstUnFlipped to lastPancake){
				updatedPancakes = updatedPancakes.updated(i, oppositePancake(updatedPancakes(i))).mkString;
			}
			return updatedPancakes;
		}
	}

	/**
	Given a current pancake state return the other one
	*/
	def oppositePancake(pancake: Char): String = {
		if(pancake == '+'){
			return "-";
		}else{
			return "+";
		}		
	}


	/**
	Determine if a pancake string is solved
	*/
	def solved(pancakes: String): Boolean = {
		return pancakes.filter({ pancake => pancake == '-'}).length == 0;
	}
}

