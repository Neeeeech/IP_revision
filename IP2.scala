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

    class Queue[T](val defaultVal: T) {
        // state s : [T]
        // init  s = []
        // abs:  s = L(first.next)
        // DTI:  last.next == null && last reachable from first && L(first) finite

        var first = Node[T](defaultVal, null)
        var last  = first

        /** Adds x to the end of queue
          *
          * pre:  none
          * post: s = s_0 ++ [x]
          * 
          * @param x element to enqueue
          */
        def enqueue(x: T): Unit = {
            last.next = Node(x, null)
            last = last.next
        }

        /** Removes & returns first in queue
          * 
          * pre:  !isEmpty
          * post: returns x s.t. x : s = s_0
          *
          * @return first in queue
          */
        def dequeue(): T = {
            require(first.next != null)
            val t = first.next.datum
            first.next = first.next.next
            t
        }

        /** Checks if empty
          * 
          * pre:  none
          * post: s = s_0 && returns s = []
          *
          * @return if empty
          */
        def isEmpty: Boolean = first.next == null

        /** Returns first in queue
          * 
          * pre:  !isEmpty
          * post: s = s_0 && returns head(s)
          *
          * @return
          */
        def front: T = {
            require(first.next != null)
            first.next.datum
        }
    }

    class Stack[T](val defaultVal: T) {
        // state s: [T]
        // init  s = []
        val head = Node(defaultVal, null)
        // abs: s = L(head.next)
        // DTI: len(L(head.next)) >= 0 && L(head) finite

        /** pushes x to top of stack
          * 
          * pre:  none
          * post: s = [x] ++ s_0
          *
          * @param x element to push
          */
        def push(x: T): Unit = head.next = Node(x, head.next)

        /** removes and returns top of stack
          * 
          * pre:  L(head.next) non-empty
          * post: returns x s.t. s_0 = [x] ++ s
          *
          * @return top of stack
          */
        def pop(): T = {
            require(!isEmpty)
            val t = head.next.datum
            head.next = head.next.next
            t
        }

        /** returns whether stack is empty
          * 
          * pre:  none
          * post: s = s_0 && returns whether stack is empty
          *
          * @return whether stack is empty
          */
        def isEmpty: Boolean = head.next == null

        /** returns top of stack without removing
          * 
          * pre:  L(head.next) non-empty
          * post: returns head(s)
          *
          * @return top of stack
          */
        def top: T = {
            require(!isEmpty)
            head.next.datum
        }
    }

    case class Node[T](val datum: T, var next: Node[T])
}