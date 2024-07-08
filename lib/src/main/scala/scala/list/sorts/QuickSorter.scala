import scala.util.Random

object QuickSorter {
  def quickSort[T](list: List[T])(implicit ord: Ordering[T]): List[T] = {
    if (list.length <= 1) {
      list
    } else {
      val pivotIndex = Random.nextInt(list.length)
      val pivot = list(pivotIndex)
      val (less, greater) = list.zipWithIndex.collect {
        case (elem, idx) if idx != pivotIndex => elem
      }.partition(ord.lt(_, pivot))
      quickSort(less) ::: pivot :: quickSort(greater)
    }
  }
}