object InsertSorter {

  // Метод для вставки элемента в отсортированный список
  def insert[T](elem: T, sortedList: List[T])(implicit ord: Ordering[T]): List[T] = {
    sortedList match {
      case Nil => List(elem)
      case head :: tail if ord.lt(elem, head) => elem :: sortedList
      case head :: tail => head :: insert(elem, tail)
    }
  }

  // Основной метод сортировки
  def sort[T](list: List[T])(implicit ord: Ordering[T]): List[T] = {
    list.foldLeft(List.empty[T])((sortedList, elem) => insert(elem, sortedList))
  }

  def main(args: Array[String]): Unit = {
    val list = List(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
    val sortedList = sort(list)
    println(sortedList)
  }
}