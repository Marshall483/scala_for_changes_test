object MergeSorter {
  // Функция для объединения двух отсортированных списков
  def merge[T](left: List[T], right: List[T])(implicit ord: Ordering[T]): List[T] = {
    (left, right) match {
      case (Nil, _) => right
      case (_, Nil) => left
      case (x :: xs, y :: ys) =>
        if (ord.lteq(x, y)) x :: merge(xs, right)
        else y :: merge(left, ys)
    }
  }

  // Функция для разделения списка на список одноэлементных списков
  def splitIntoSublists[T](list: List[T]): List[List[T]] = {
    list.map(List(_))
  }

  // Функция для объединения пар списков в заданный список списков
  def mergePairs[T](lists: List[List[T]])(implicit ord: Ordering[T]): List[List[T]] = {
    lists match {
      case Nil => Nil
      case x :: Nil => List(x)
      case x :: y :: rest => merge(x, y) :: mergePairs(rest)
    }
  }

  // Основная функция для сортировки списка с использованием сортировки слиянием
  def mergeSort[T](list: List[T])(implicit ord: Ordering[T]): List[T] = {
    def mergeSortIter(lists: List[List[T]]): List[T] = {
      lists match {
        case Nil => Nil
        case x :: Nil => x
        case _ => mergeSortIter(mergePairs(lists))
      }
    }

    mergeSortIter(splitIntoSublists(list))
  }
}