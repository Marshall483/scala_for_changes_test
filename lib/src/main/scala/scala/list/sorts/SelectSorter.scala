object SelectSorter {

  def sort[T](list: List[T])(implicit ord: Ordering[T]): List[T] = {
    @annotation.tailrec
    def selectSort(remaining: List[T], sorted: List[T]): List[T] = {
      if (remaining.isEmpty) sorted
      else {
        val maxElem = remaining.max
        val index = remaining.indexOf(maxElem)
        val result = if (index >= 0) remaining.patch(index, Nil, 1) else list
        selectSort(result, maxElem :: sorted)
      }
    }

    selectSort(list, Nil)
  }
}