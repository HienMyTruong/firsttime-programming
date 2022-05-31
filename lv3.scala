
import scala.io.StdIn.readLine
  
object tarningsspel {
  def slowPrint(str: String): Unit = 
    str.foreach(i => {print(i); Thread.sleep(50)})
    println()

  def readChoice(msg: String, options: Vector[String]): String =
    options.indices.foreach(i => slowPrint(s"$i: ${options(i)}" +"\n"))    //i =tar varje index av samling os 
    val selected = readLine(msg).toInt                 //io.StdIn. kan tas bort
    options(selected)
  
  def roll(): Int =
    (math.random()*6 + 1).toInt

  def rollSet(size: Int): IndexedSeq[Int]= 
    val set: IndexedSeq[Int]= for i <- 1 to 5 yield roll()
    set     //return
  
  def rollmax(size: Int): IndexedSeq[Int]= 
    val set: IndexedSeq[Int]= for i <- 1 to 5 yield 6
    set //return
  
  def run(cheatEnabled: Boolean): Unit = 
    var currSet = rollSet(5)
    if cheatEnabled then currSet = rollmax(5)    //currset blir rollmax
    def summa = currSet.sum
    slowPrint("Du kastar 5 tärningar. Ditt kast är: \n")
    Helper.vectorPrint(currSet)
    slowPrint(s"Din summa är: $summa")
    if summa != 30 then slowPrint("\n Du förlorade :( \n Spela igen!") 
    if summa == 30 then slowPrint("\n Du har vunnit! \n Spela igen!")
  
  
  def playGame(Spelare: String): Unit =
    val os = Vector("kasta tärningarna",  "dö")
    val choice = readChoice(s"\n $Spelare väljer: ", os)
    if (choice == os(1)) then 
      run(true) 
    else 
      run(false)
    
  @main
  def Start: Unit = 
    slowPrint("Du ska kasta 5 tärningar. Du vinner om du får 5st sexor. Skriv ditt namn: ")
    var Spelare: String = readLine("")
    while true do                //true = skit i allt bara fortsätt
      playGame(Spelare) 
      slowPrint("\n \n")


}

object Helper {
  //def multiPrint( f: () => Int, count:Int) = 
    //(1 to count).foreach(_ => println(f()))
  def vectorPrint(vec: IndexedSeq[Int]) = 
          vec.foreach{ i => println(i) }
 
}
