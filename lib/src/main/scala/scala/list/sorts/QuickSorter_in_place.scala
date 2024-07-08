import scala.util.Random

object QuickSorter_in_place {
  def quickSort[T](arr: Array[T])(implicit ord: Ordering[T]): Unit = {
    def swap(i: Int, j: Int): Unit = {
      val temp = arr(i)
      arr(i) = arr(j)
      arr(j) = temp
    }

    def partition(low: Int, high: Int): Int = {
      val pivotIndex = Random.nextInt(high - low + 1) + low
      val pivot = arr(pivotIndex)
      swap(pivotIndex, high)
      var i = low
      for (j <- low until high) {
        if (ord.lt(arr(j), pivot)) {
          swap(i, j)
          i += 1
        }
      }
      swap(i, high)
      i
    }

    def quickSortRec(low: Int, high: Int): Unit = {
      if (low < high) {
        val pi = partition(low, high)
        quickSortRec(low, pi - 1)
        quickSortRec(pi + 1, high)
      }
    }

    quickSortRec(0, arr.length - 1)
  }
}