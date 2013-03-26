package scala.fp.chapter3

import org.scalatest.FunSuite

class Exercises extends FunSuite {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def tail[A](l: List[A]): List[A] = l match {
      case Nil => throw new IllegalStateException("Tail of empty list")
      case Cons(h, Nil) => throw new IllegalStateException("List only contains one element")
      case Cons(h, t) => t
    }

    def drop[A](l: List[A], n: Int): List[A] =
      if (n <= 0) l
      else l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }

    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
      l match {
        case Cons(h, t) if f(h) => dropWhile(t)(f)
        case _ => l
      }

    def setHead[A](l: List[A], e: A): List[A] =
      l match {
        case Nil => Cons(e, Nil)
        case Cons(_, t) => Cons(e, t)
      }

    def init[A](l: List[A]): List[A] =
      l match {
        case Nil => sys.error("init of empty list")
        case Cons(_, Nil) => Nil
        case Cons(x, xs) => Cons(x, init(xs))
      }

    def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def sum2(l: List[Int]) = foldRight(l, 0.0)(_ + _)

    def product2(l: List[Double]) = foldRight(l, 1.0)(_ * _)

    def length[A](l: List[A]): Int = foldRight[A, Int](l, 0)((_, b: Int) => b + 1)


    @annotation.tailrec
    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

    def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

    def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

    def length3[A](l: List[A]): Int = foldLeft(l, 0)((acc, h) => acc + 1)

    def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

    def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(reverse(l), z)((b, a) => f(a, b))

    def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

    def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B) =
      foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

    def append[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(Cons(_, _))

    def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)

    def addOne(l: List[Int]): List[Int] =
      l match {
        case Nil => l
        case Cons(x, xs) => Cons(x + 1, addOne(xs))
      }

    def addOne_1(l: List[Int]): List[Int] = foldRight(l, List[Int]())((h, acc) => Cons(h + 1, acc))

    def asString(l: List[Double]): List[String] = foldRight(l, List[String]())((h, t) => Cons(h.toString, t))

    // Will overflow for large lists
    def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

    def map_1[A, B](l: List[A])(f: A => B): List[B] = foldRightViaFoldLeft(l, Nil: List[B])((h, t) => Cons(f(h), t))

    def map_2[A, B](l: List[A])(f: A => B): List[B] = {
      val buf = new collection.mutable.ListBuffer[B]
      def go(l: List[A]): Unit = l match {
        case Nil => ()
        case Cons(h, t) => buf += f(h); go(t)
      }
      go(l)
      List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
    }

    def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

    def filter_1[A](l: List[A])(f: A => Boolean): List[A] =
      foldRightViaFoldLeft(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

    def filter_2[A](l: List[A])(f: A => Boolean): List[A] = {
      val buf = new collection.mutable.ListBuffer[A]
      def go(l: List[A]): Unit = l match {
        case Nil => ()
        case Cons(h, t) => if (f(h)) buf += h; go(t)
      }
      go(l)
      List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
    }

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

    def filter_3[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if (f(a)) List(a) else Nil)

    def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addPairwise(xs, ys))
    }

    def zip[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zip(xs, ys)(f))
    }

    def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
      false
    }
  }

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    def size[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

    def maximum(tree: Tree[Int]): Int = tree match {
      case Leaf(n) => n
      case Branch(l, r) => maximum(l) max maximum(r)
    }

    def depth[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(n) => Leaf(f(n))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    /*
      Like `foldRight` for lists, `fold` receives a "handler" for each of the data constructors of the type,
      and recursively accumulates some value using these handlers. As with `foldRight`, `fold(t)(Leaf(_))(Branch(_,_)) == t`,
      and we can use this function to implement just about any recursive function that would otherwise be defined by pattern matching.
    */
    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
      case Leaf(n) => f(n)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def sizeViaFold[A](tree: Tree[A]): Int = fold(tree)(a => 1)(1 + _ + _)

    def maxViaFold(tree: Tree[Int]): Int = fold(tree)(a => a)(_ max _)

    def depthViaFold[A](tree: Tree[A]): Int = fold(tree)(a => 0)((d1, d2) => (1 + (d1 max d2)))

    /*
    Notice the type annotation required on the expression `Leaf(f(a))`. Without this annotation, we get an error like this:

    type mismatch;
      found   : fpinscala.datastructures.Branch[B]
      required: fpinscala.datastructures.Leaf[B]
      fold(t)(a => Leaf(f(a)))(Branch(_,_))
                                     ^

      This error is an unfortunate consequence of Scala using subtyping to encode algebraic data types.
      Without the annotation, the result type of the fold gets inferred as `Leaf[B]` and it is then expected that the second argument to `fold` will return
      `Leaf[B]`, which it does not (it returns `Branch[B]`). Really, we would prefer if Scala would infer `Tree[B]` as the result type in both cases.
      When working with algebraic data types in Scala, it is somewhat common to define helper functions that simply call the corresponding data constructors
      but give the less specific result type:

     def leaf[A](a: A): Tree[A] = Leaf(a)
     def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
    */
    def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
  }

  test("Exercice 1 - Pattern matching") {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    assert(3 == x)
  }

  test("Exercice 2 - tail") {
    assert(List(2, 3) == List.tail(List(1, 2, 3)))
  }

  test("Exercice 3 - drop") {
    assert(List(3, 4, 5) == List.drop(List(1, 2, 3, 4, 5), 2))
  }

  test("Exercise 4 - dropWhile") {
    assert(List(3, 4, 5) == List.dropWhile(List(1, 2, 3, 4, 5))(_ <= 2))
  }

  test("Exercise 5 - setHead") {
    assert(List(1, 2, 3) == List.setHead(List(2, 2, 3), 1))
  }

  test("Exercise 6 - init") {
    assert(List(1, 2, 3, 4, 5) == List.init(List(1, 2, 3, 4, 5, 6)))
  }

  test("Exercise 7 - Paper exercise") {

  }

  test("Exercise 8 - Product") {
    /*Can product implemented using foldRight immediately
      halt the recursion and return 0.0 if it encounters a 0.0? Why or why not?
    Consider how any short-circuiting might work if you call foldRight with a
    large list.*/

    /*Answer: No*/
  }

  test("Exercise 9 - Nil and Cons and foldRight") {
    List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))

    /*
    We get back the original list! Why is that? As we mentioned earlier,
    one way of thinking about what `foldRight` "does" is it replaces the `Nil` constructor of the list with the `z` argument,
    and it replaces the `Cons` constructor with the given function, `f`. If we just supply `Nil` for `z` and `Cons` for `f`,
    then we get back the input list.
     */
  }

  test("Exercise 10 - Compute the length of a list using foldRight") {
    assert(5 == List.length(List(1, 2, 3, 4, 5)))
  }

  test("Exercise 11 - foldRight is not tail recursive") {
    assert(15 == List.foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _))
  }

  test("Exercise 12 - using foldLeft") {
    assert(15 == List.sum3(List(1, 2, 3, 4, 5)))
    assert(120 == List.product3(List(1, 2, 3, 4, 5)))
    assert(5 == List.length3(List(1, 2, 3, 4, 5)))
  }

  test("Exercise 13 - Reverse list") {
    assert(List(5, 4, 3, 2, 1) == List.reverse(List(1, 2, 3, 4, 5)))
  }

  /*
  The implementation of `foldRight` in terms of `reverse` and `foldLeft` is a common trick for avoiding stack overflows when implementing a
  strict `foldRight` function as we've done in this chapter. (We will revisit this in a later chapter, when we discuss laziness).
  The other implementations build up a chain of functions which, when called,
  results in the operations being performed with the correct associativity.
  */
  test("Exercise 14 - Write foldLeft in terms of foldRight") {

  }

  /*
  `append` simply replaces the `Nil` constructor of the first list with the second list, which is exactly the operation performed by `foldRight`.

  Since `append` takes time proportional to its first argument, and this first argument never grows because of the right-associativity of `foldRight`,
  this function is linear in the total length of all lists. You may want to try tracing the execution of the implementation on paper to convince
  yourself that this works.
 */
  test("Exercise 15 - append") {
    assert(List(1, 2, 3, 4, 5, 6) == List.append(List(1, 2, 3), List(4, 5, 6)))
  }

  /*
    Notice we are simply referencing the `append` function, without writing something like `(x,y) => append(x,y)` or `append(_,_)`.
  In Scala there is a rather arbitrary distinction between functions defined as _methods_, which are introduced with the `def` keyword,
  and function values, which are the first-class objects we can pass to other functions, put in collections, and so on.
  This is a case where Scala lets us pretend the distinction doesn't exist. In other cases, you'll be forced to write `append _`
  (to convert a `def` to a function value) or even `(x: List[A], y: List[A]) => append(x,y)` if the function is polymorphic and
  the type arguments are not known.
   */
  test("Exercise 16 - concat") {
    assert(List(1, 2, 3, 4, 5, 6, 7, 8, 9) == List.concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))))
  }

  test("Exercise 17 - Transform list of integers by adding 1 to each element") {
    assert(List(2, 3, 4, 5, 6) == List.addOne(List(1, 2, 3, 4, 5)))
    assert(List(2, 3, 4, 5, 6) == List.addOne_1(List(1, 2, 3, 4, 5)))
  }

  test("Exercise 18 - Turn each value of a List[Double] to a String") {
    assert(List("1.0", "2.0", "3.0", "4.0", "5.0") == List.asString(List(1, 2, 3, 4, 5)))
  }

  /*
  A natural solution is using `foldRight`, however this will stack overflow for large lists.
  We can use `foldRightViaFoldLeft` to avoid the stack overflow (variation 1), but more commonly, with our
  current implementation of `List`, `map` will just be implemented using local mutation (variation 2).
  Again, notice that the mutation is not observable outside the function, since we are only mutating a buffer that we have allocated.
  */
  test("Exercise 19 - Write a function map, that generalizes modifying each element in a list while maintaining the structure of the list") {

  }

  test("Exercise 20 - Write a function filter that removes elements from a list unless they satisfy a given predicate") {
    assert(List(2, 4, 6, 8) == List.filter(List(1, 2, 3, 4, 5, 6, 7, 8))(_ % 2 == 0))
  }

  test("Exercise 21 - Write a function flatMap, that works like map expect that the function given will return a list instead of a single result") {
    assert(List(1, 1, 2, 2, 3, 3) == List.flatMap(List(1, 2, 3))(i => List(i, i)))
  }

  test("Exercise 22 - Use flatMap to implement filter") {
    assert(List(2, 4, 6, 8) == List.filter_3(List(1, 2, 3, 4, 5, 6, 7, 8))(_ % 2 == 0))
  }

  test("Exercise 23 - Write a function that accepts two lists and constructs a new list by adding corresponding elements") {
    assert(List(5, 7, 9) == List.addPairwise(List(1, 2, 3), List(4, 5, 6)))
  }

  test("Exercise 24 - Generalize exercise 23") {
    assert(List("ad", "be", "cf") == List.zip(List("a", "b", "c"), List("d", "e", "f"))(_ + _))
  }

  test("Exercise 25 - hasSubsequence") {

  }

  test("Exercise 26 - Count the number of nodes in a tree") {
    assert(5 == Tree.size(Branch(Branch(Leaf(), Leaf()), Leaf())))
  }

  test("Exercise 27 - Write a function maximum that return the maximum element in a Tree[Int]") {
    assert(7 == Tree.maximum(Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(7), Leaf(6)))))
  }

  test("Exercise 28 - Write a function depth that returns the maximum path length from root of a tree to any leaf") {
    assert(3 == Tree.depth(Branch(Branch(Leaf(2), Branch(Leaf(3), Leaf(3))), Branch(Leaf(7), Leaf(6)))))
  }

  test("Exercise 29 - Write a function map, that modifies each element in a tree with a given function") {
    assert(Branch(Branch(Leaf(2), Branch(Leaf(3), Leaf(3))), Branch(Leaf(7), Leaf(6))) == Tree.map(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(2))), Branch(Leaf(6), Leaf(5))))(_ + 1))
  }

  test("Exercuse 30 - Generalize size, maximum, depth and map, by writing a new function fold that abstracts over their similarities") {
    assert(5 == Tree.sizeViaFold(Branch(Branch(Leaf(), Leaf()), Leaf())))
    assert(7 == Tree.maxViaFold(Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(7), Leaf(6)))))
    assert(3 == Tree.depthViaFold(Branch(Branch(Leaf(2), Branch(Leaf(3), Leaf(3))), Branch(Leaf(7), Leaf(6)))))
    assert(Branch(Branch(Leaf(2), Branch(Leaf(3), Leaf(3))), Branch(Leaf(7), Leaf(6))) == Tree.mapViaFold(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(2))), Branch(Leaf(6), Leaf(5))))(_ + 1))
  }
}


