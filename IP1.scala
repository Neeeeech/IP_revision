object IP1 {
    /** Recursively raises x to the nth power
      *
      * @param x base
      * @param n exponent
      * @return x^n
      */
    def fastExp(x: Int, n: Int): Int = {
        if (n == 0) return 1
        if (n % 2 == 1) return x * fastExp(x*x, n/2)
        fastExp(x*x, n/2)
    }

    /** Iteratively raises x to the nth power
      *
      * @param x base
      * @param n exponent
      * @return x^n
      */
    def fastExpLoop(x: Int, n: Int): Int = {
        var (res, y, m) = (1, x, n)
        // I: x^n = res * (y^m) && m >= 0
        while (m > 0) {
            if (m % 2 == 1) res *= y
            y *= y; m /= 2
        }
        res
    }

    /** Recursively binary searches for an element in a sorted list
      *
      * @param a array to search through
      * @param x element to find
      * @param l left inclusive bound
      * @param r right exclusive bound
      * @return None if not found, Some(index) if found
      */
    def binSearch(a: Array[Int], x: Int, l: Int, r: Int): Option[Int] = {
        require(0 <= l && r <= a.length)
        if (l == r) return None
        val m = l + (r - l)/2
        if (a(m) == x) return Some(m)
        if (a(m) > x) return binSearch(a, x, l, m)
        binSearch(a, x, m+1, r)
    }

    /** Iteratively binary searches for an element in a sorted list
      *
      * @param a array to search through
      * @param x element to find
      * @param l left inclusive bound
      * @param r right exclusive bound
      * @return None if not found, Some(index) if found
      */
    def binSearchLoop(a: Array[Int], x: Int, l: Int, r: Int): Option[Int] = {
        require(0 <= l && r <= a.length)

        // Checking if x is even within a[l,r)
        if (a(l) > x || (r != a.length && x >= a(r)))
            return None
        
        // Searching within the a[l,r)
        var (left, right) = (l, r)
        // I: a = a_0  &&  left < right  &&  a(left) <= x < a(right) where a(a.length) = infinity
        while (left + 1 < right) {
            val m = left + (right - left) / 2
            if (a(m) <= x) left  = m
            else           right = m
        }
        // From I & not C: left < right <= left + 1, so left + 1 = right
        // a(left) <= x < a(left + 1), so if x is in the range, a(left) = x

        if (a(left) == x) return Some(left)
        None
    }

    def swap[T](a: Array[T], x: Int, y: Int): Unit = {
        require(0 <= x && x < a.length && 0 <= y && y < a.length)
        val t = a(x); a(x) = a(y); a(y) = t
    }

    /** Partitions a[l, r) around a(l).
      *
      * @param a the array
      * @param l the left inclusive bound
      * @param r the right inclusive bound
      * @return index separating the partitions
      */
    def partition(a: Array[Int], l: Int, r: Int): Int = {
        require(0 <= l && l < r && r <= a.length)
        var (x, y) = (l, r)
        // I: a[l, x) < a(x) = a_0(l) <= a[y, r) && x + 1 <= y && a = a_0
        while (x + 1 < y) {
            if (a(x + 1) < a(x)) {
                swap(a, x, x+1)
                x += 1
            } else if (a(x) <= a(y-1)) {
                y -= 1
            } else {
                // x+1 != y-1, as a(x+1) >= a(x), but a(x) > a(y-1), so a(y-1) < a(x+1)
                swap(a, x+1, y-1)
                swap(a, x, x+1)
                x += 1; y -= 1
            }
        }
        // x + 1 = y, so a[l,x) < a(x) = a_0(l) <= a[x + 1, r) && a = a_0
        x
    }

    /** quickSorts the array between l and r
      *
      * @param a the array
      * @param l left bound inclusive
      * @param r right bound exclusive
      */
    def quickSort(a: Array[Int], l: Int, r: Int): Unit = {
        // QuickSort takes on average O(n log n) time, with O(n^2) in the worst case
        // but, you could find the median in O(n) time to force the partitions to be equal
        // this would force quicksort to run in O(n log n) time, by Master's Theorem (T(n) = 2T(n/2) + O(n))
        // but I won't do it here bc finding the median in O(n) time is difficult and I doubt we'll be asked to do it

        var x = l
        // I: x <= r && a is a permutation of a_0 && a[l, x) sorted, and a[l, x) < a[x, r)
        while (x < r) {
            val p = partition(a, x, r)
            quickSort(a, l, p)
            x = p + 1
        }
        // x = r, so a[l,r) is sorted
    }
}