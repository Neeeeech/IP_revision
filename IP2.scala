object IP2 {
    class IntLinkedList {
        // state s: [Int]
        // init  s = Nil
        // abs: s = L(head.next)
        // DTI: L(head) finite
        val head: Node[Int] = Node(0, null)

        // Define list reachable from node n as:
        // L(from, to) = if (from == to) Nil otherwise from.datum :: L(from.next, to)
        // and L(from) = L(from, null)

        /** Returns the node preceding one containing x
          *
          * pre:  none
          * post: returns as below
          * 
          * @param x element to find
          * @return node preceding one containing x, or null if x not in list
          */
        private def find(x: Int): Node[Int] = {
            var n = head
            // I: x not in L(head.next, n.next)
            while (n.next != null && n.next.datum != x)
                n = n.next
            // Case 1: n.next == null
            //  x not in L(head.next, null), so x not in s
            // Case 2: n.next.datum == x
            //  n is what we need
            n
        }
        
        /** Adds x to the linked list
          *
          * @param x element to add
          * @return 
          */
        def add(x: Int): Unit = head.next = Node(x, head.next)

        /** Deletes x if possible. Returns whether x deleted
          *
          * @param x element to delete
          * @return whether deletion was possible
          */
        def delete(x: Int): Boolean = find(x) match {
            case n: Node[Int] if (n.next != null) => { n.next = n.next.next; true}
            case _ => false
        }

        override def toString(): String = {
            var n = head.next; var s = "["
            while (n != null) {
                if (n.next != null)
                    s += f"${n.datum},"
                else
                    s += n.datum.toString
                n = n.next
            }
            (s + "]")
        }
    }

    case class Node[T](val datum: T, var next: Node[T])
}