val a = "A"
val b = 'B'
def myList: List[Char] = List('A','D','C')
type Pair = (Char, Int)

def newList = myList.map{ case 'D' => 'B'; case x => x}
newList.tail.head
def myList2: List[(Char,Int)] = List(('A',5),('B',1),('C',3))
val remain = List('B')
def incrPair(lookChar: Char): Pair => (Char,Int) = {
  def incrPairSupp(ps: (Char, Int)): (Char, Int) = {
    ps match {
      case (x, n) => if(x==lookChar) (x,n+1) else (x,n)
    }
  }
  incrPairSupp
}
incrPair('A')(('B',1))
def exList = myList2.map{case (myC,nc) => if(myC==remain.head) (remain.head,nc+1)
                          else (myC, nc)}
def exList2 = myList2.map{incrPair(remain.head)}
exList2.head
exList2.tail.head
exList2.tail.tail.head
def PairCompare(x: Pair, y: Pair): Boolean = x._2 < y._2
def exList3 = myList2.sortWith(PairCompare)
exList3.head
PairCompare(('A',5),('B',7))