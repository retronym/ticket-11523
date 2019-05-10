/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package example


/** A partial function of type `PartialFunction[A, B]` is a unary function
 *  where the domain does not necessarily include all values of type `A`.
 *  The function `isDefinedAt` allows to test dynamically if a value is in
 *  the domain of the function.
 *
 *  Even if `isDefinedAt` returns true for an `a: A`, calling `apply(a)` may
 *  still throw an exception, so the following code is legal:
 *
 *  {{{
 *  val f: PartialFunction[Int, Any] = { case _ => 1/0 }
 *  }}}
 *
 *  It is the responsibility of the caller to call `isDefinedAt` before
 *  calling `apply`, because if `isDefinedAt` is false, it is not guaranteed
 *  `apply` will throw an exception to indicate an error condition. If an
 *  exception is not thrown, evaluation may result in an arbitrary value.
 *
 *  The main distinction between `PartialFunction` and [[scala.Function1]] is
 *  that the user of a `PartialFunction` may choose to do something different
 *  with input that is declared to be outside its domain. For example:
 *
 *  {{{
 *  val sample = 1 to 10
 *  val isEven: PartialFunction[Int, String] = {
 *    case x if x % 2 == 0 => x+" is even"
 *  }
 *
 *  // the method collect can use isDefinedAt to select which members to collect
 *  val evenNumbers = sample collect isEven
 *
 *  val isOdd: PartialFunction[Int, String] = {
 *    case x if x % 2 == 1 => x+" is odd"
 *  }
 *
 *  // the method orElse allows chaining another partial function to handle
 *  // input outside the declared domain
 *  val numbers = sample map (isEven orElse isOdd)
 *  }}}
 *
 *  @note Optional [[Function]]s, [[PartialFunction]]s and extractor objects
 *        can be converted to each other as shown in the following table.
 *
 *        | How to convert ... | to a [[PartialFunction]] | to an optional [[Function]] | to an extractor |
 *        | :---:  | ---  | --- | --- |
 *        | from a [[PartialFunction]] | [[Predef.identity]] | [[lift]] | [[Predef.identity]] |
 *        | from optional [[Function]] | [[Function.UnliftOps#unlift]] or [[Function.unlift]] | [[Predef.identity]] | [[Function.UnliftOps#unlift]] |
 *        | from an extractor | `{ case extractor(x) => x }` | `extractor.unapply _` | [[Predef.identity]] |
 *
 *  @author  Martin Odersky, Pavel Pavlov, Adriaan Moors
 *  @since   1.0
 */
trait PartialFunction[-A, +B] extends Function1[A,B] { self =>
  import PartialFunction._

  /** Tries to extract a `B` from an `A` in a pattern matching expression. */
  def unapply(a: A): Option[B] = lift(a)

  /** Returns an extractor object with a `unapplySeq` method, which extracts each element of a sequence data.
   *
   *  @example {{{
   *           val firstChar: String => Option[Char] = _.headOption
   *
   *           Seq("foo", "bar", "baz") match {
   *             case firstChar.unlift.elementWise(c0, c1, c2) =>
   *               println(s"$c0, $c1, $c2") // Output: f, b, b
   *           }
   *           }}}
   */
  def elementWise = new ElementWiseExtractor(this)

  /** Checks if a value is contained in the function's domain.
   *
   *  @param  x   the value to test
   *  @return `'''true'''`, iff `x` is in the domain of this function, `'''false'''` otherwise.
   */
  def isDefinedAt(x: A): Boolean

  /** Composes this partial function with a fallback partial function which
   *  gets applied where this partial function is not defined.
   *
   *  @param   that    the fallback function
   *  @tparam  A1      the argument type of the fallback function
   *  @tparam  B1      the result type of the fallback function
   *  @return  a partial function which has as domain the union of the domains
   *           of this partial function and `that`. The resulting partial function
   *           takes `x` to `this(x)` where `this` is defined, and to `that(x)` where it is not.
   */
  def orElse[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]): PartialFunction[A1, B1] =
    new OrElse[A1, B1] (this, that)
  //TODO: why not overload it with orElse(that: F1): F1?

  /**  Composes this partial function with a transformation function that
   *   gets applied to results of this partial function.
   *
   *   If the runtime type of the function is a `PartialFunction` then the
   *   other `andThen` method is used (note its cautions).
   *
   *   @param  k  the transformation function
   *   @tparam C  the result type of the transformation function.
   *   @return a partial function with the domain of this partial function,
   *           possibly narrowed by the specified function, which maps
   *           arguments `x` to `k(this(x))`.
   */
  override def andThen[C](k: B => C): PartialFunction[A, C] = ???

  /**
   * Composes this partial function with another partial function that
   * gets applied to results of this partial function.
   *
   * Note that calling [[isDefinedAt]] on the resulting partial function may apply the first
   * partial function and execute its side effect. It is highly recommended to call [[applyOrElse]]
   * instead of [[isDefinedAt]] / [[apply]] for efficiency.
   *
   * @param  k  the transformation function
   * @tparam C  the result type of the transformation function.
   * @return a partial function with the domain of this partial function narrowed by
   *         other partial function, which maps arguments `x` to `k(this(x))`.
   */
  def andThen[C](k: PartialFunction[B, C]): PartialFunction[A, C] =
    new Combined[A, B, C](this, k)

  /**
   * Composes another partial function `k` with this partial function so that this
   * partial function gets applied to results of `k`.
   *
   * Note that calling [[isDefinedAt]] on the resulting partial function may apply the first
   * partial function and execute its side effect. It is highly recommended to call [[applyOrElse]]
   * instead of [[isDefinedAt]] / [[apply]] for efficiency.
   *
   * @param  k  the transformation function
   * @tparam R  the parameter type of the transformation function.
   * @return a partial function with the domain of other partial function narrowed by
   *         this partial function, which maps arguments `x` to `this(k(x))`.
   */
  def compose[R](k: PartialFunction[R, A]): PartialFunction[R, B] =
    new Combined[R, A, B](k, this)

  /** Turns this partial function into a plain function returning an `Option` result.
   *  @see     Function.unlift
   *  @return  a function that takes an argument `x` to `Some(this(x))` if `this`
   *           is defined for `x`, and to `None` otherwise.
   */
  def lift: A => Option[B] = new Lifted(this)

  /** Applies this partial function to the given argument when it is contained in the function domain.
   *  Applies fallback function where this partial function is not defined.
   *
   *  Note that expression `pf.applyOrElse(x, default)` is equivalent to
   *  {{{ if(pf isDefinedAt x) pf(x) else default(x) }}}
   *  except that `applyOrElse` method can be implemented more efficiently.
   *  For all partial function literals the compiler generates an `applyOrElse` implementation which
   *  avoids double evaluation of pattern matchers and guards.
   *  This makes `applyOrElse` the basis for the efficient implementation for many operations and scenarios, such as:
   *
   *  - combining partial functions into `orElse`/`andThen` chains does not lead to
   *    excessive `apply`/`isDefinedAt` evaluation
   *  - `lift` and `unlift` do not evaluate source functions twice on each invocation
   *  - `runWith` allows efficient imperative-style combining of partial functions
   *    with conditionally applied actions
   *
   *  For non-literal partial function classes with nontrivial `isDefinedAt` method
   *  it is recommended to override `applyOrElse` with custom implementation that avoids
   *  double `isDefinedAt` evaluation. This may result in better performance
   *  and more predictable behavior w.r.t. side effects.
   *
   *  @param  x       the function argument
   *  @param default  the fallback function
   *  @return   the result of this function or fallback function application.
   *  @since   2.10
   */
  def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 =
    if (isDefinedAt(x)) apply(x) else default(x)

  /** Composes this partial function with an action function which
   *  gets applied to results of this partial function.
   *  The action function is invoked only for its side effects; its result is ignored.
   *
   *  Note that expression `pf.runWith(action)(x)` is equivalent to
   *  {{{ if(pf isDefinedAt x) { action(pf(x)); true } else false }}}
   *  except that `runWith` is implemented via `applyOrElse` and thus potentially more efficient.
   *  Using `runWith` avoids double evaluation of pattern matchers and guards for partial function literals.
   *  @see `applyOrElse`.
   *
   *  @param   action  the action function
   *  @return  a function which maps arguments `x` to `isDefinedAt(x)`. The resulting function
   *           runs `action(this(x))` where `this` is defined.
   *  @since   2.10
   */
  def runWith[U](action: B => U): A => Boolean = null
}

/** A few handy operations which leverage the extra bit of information
 *  available in partial functions.  Examples:
 *  {{{
 *  import PartialFunction._
 *
 *  def strangeConditional(other: Any): Boolean = cond(other) {
 *    case x: String if x == "abc" || x == "def"  => true
 *    case x: Int => true
 *  }
 *  def onlyInt(v: Any): Option[Int] = condOpt(v) { case x: Int => x }
 *  }}}
 *
 *  @author  Paul Phillips
 *  @since   2.8
 */
object PartialFunction {

  final class ElementWiseExtractor[-A, +B] private[PartialFunction] (private val pf: PartialFunction[A, B]) extends AnyVal {
    def unapplySeq(seq: Seq[A]): Option[Seq[B]] = {
      Some(seq.map {
        case pf(b) => b
        case _ => return None
      })
    }
  }

  /** Composite function produced by `PartialFunction#orElse` method
   */
  private class OrElse[-A, +B] (f1: PartialFunction[A, B], f2: PartialFunction[A, B])
    extends example.AbstractPartialFunction[A, B] with Serializable {
    def isDefinedAt(x: A) = f1.isDefinedAt(x) || f2.isDefinedAt(x)

    override def apply(x: A): B = ???

    override def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 = ???

    override def orElse[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]) =
      new OrElse[A1, B1] (f1, f2 orElse that)

    override def andThen[C](k: B => C) =
      new OrElse[A, C] (f1 andThen k, f2 andThen k)
  }

  /** Composite function produced by `PartialFunction#andThen` method
   */
  private class AndThen[-A, B, +C] (pf: PartialFunction[A, B], k: B => C) extends PartialFunction[A, C] with Serializable {
    def isDefinedAt(x: A) = pf.isDefinedAt(x)

    def apply(x: A): C = k(pf(x))

    override def applyOrElse[A1 <: A, C1 >: C](x: A1, default: A1 => C1): C1 = ???
  }

  /** Composite function produced by `PartialFunction#andThen` method
   */
  private class Combined[-A, B, +C] (pf: PartialFunction[A, B], k: PartialFunction[B, C]) extends PartialFunction[A, C] with Serializable {
    def isDefinedAt(x: A): Boolean = ???

    def apply(x: A): C = k(pf(x))

    override def applyOrElse[A1 <: A, C1 >: C](x: A1, default: A1 => C1): C1 = ???
  }

  /** To implement patterns like {{{ if(pf isDefinedAt x) f1(pf(x)) else f2(x) }}} efficiently
   *  the following trick is used:
   *
   *  To avoid double evaluation of pattern matchers & guards `applyOrElse` method is used here
   *  instead of `isDefinedAt`/`apply` pair.
   *
   *  After call to `applyOrElse` we need both the function result it returned and
   *  the fact if the function's argument was contained in its domain. The only degree of freedom we have here
   *  to achieve this goal is tweaking with the continuation argument (`default`) of `applyOrElse` method.
   *  The obvious way is to throw an exception from `default` function and to catch it after
   *  calling `applyOrElse` but I consider this somewhat inefficient.
   *
   *  I know only one way how you can do this task efficiently: `default` function should return unique marker object
   *  which never may be returned by any other (regular/partial) function. This way after calling `applyOrElse` you need
   *  just one reference comparison to distinguish if `pf isDefined x` or not.
   *
   *  This correctly interacts with specialization as return type of `applyOrElse`
   *  (which is parameterized upper bound) can never be specialized.
   *
   *  Here `fallback_pf` is used as both unique marker object and special fallback function that returns it.
   */
  private[this] val fallback_pf: PartialFunction[Any, Any] = null
  private def checkFallback[B] = fallback_pf.asInstanceOf[PartialFunction[Any, B]]
  private def fallbackOccurred[B](x: B) = (fallback_pf eq x.asInstanceOf[AnyRef])

  private class Lifted[-A, +B] (val pf: PartialFunction[A, B])
    extends scala.runtime.AbstractFunction1[A, Option[B]] with Serializable {

    def apply(x: A): Option[B] = null
  }

  private class Unlifted[A, B] (f: A => Option[B]) extends scala.runtime.AbstractPartialFunction[A, B] with Serializable {
    def isDefinedAt(x: A): Boolean = f(x).isDefined

    override def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 = {
      val z = f(x)
      if (!z.isEmpty) z.get else default(x)
    }

    override def lift = f
  }

  private def unlifted[A, B](f: A => Option[B]): PartialFunction[A, B] = null
  /**  Converts an ordinary function to a partial function. Note that calling `isDefinedAt(x)` on
   *   this partial function will return `true` for every `x`.
   *   @param  f  an ordinary function
   *   @return    a partial function which delegates to the ordinary function `f`
   */
  def fromFunction[A, B](f: A => B): PartialFunction[A, B] = null

  private[this] val constFalse: Any => Boolean = { _ => false}

  private[this] val empty_pf: PartialFunction[Any, Nothing] = null

  /** The partial function with empty domain.
   *  Any attempt to invoke empty partial function leads to throwing [[scala.MatchError]] exception.
   *  @since   2.10
   */
  def empty[A, B] : PartialFunction[A, B] = empty_pf

  /** Creates a Boolean test based on a value and a partial function.
   *  It behaves like a 'match' statement with an implied 'case _ => false'
   *  following the supplied cases.
   *
   *  @param  x   the value to test
   *  @param  pf  the partial function
   *  @return true, iff `x` is in the domain of `pf` and `pf(x) == true`.
   */
  def cond[T](x: T)(pf: PartialFunction[T, Boolean]): Boolean = pf.applyOrElse(x, constFalse)

  /** Transforms a PartialFunction[T, U] `pf` into Function1[T, Option[U]] `f`
   *  whose result is `Some(x)` if the argument is in `pf`'s domain and `None`
   *  otherwise, and applies it to the value `x`.  In effect, it is a
   *  `'''match'''` statement which wraps all case results in `Some(_)` and
   *  adds `'''case''' _ => None` to the end.
   *
   *  @param  x     the value to test
   *  @param  pf    the PartialFunction[T, U]
   *  @return `Some(pf(x))` if `pf isDefinedAt x`, `None` otherwise.
   */
  def condOpt[T,U](x: T)(pf: PartialFunction[T, U]): Option[U] = pf.lift(x)
}



object Function1 {

  implicit final class UnliftOps[A, B] private[Function1](private val f: A => Option[B]) extends AnyVal {
    /** Converts an optional function to a partial function.
     *
     * @example Unlike [[Function.unlift]], this [[UnliftOps.unlift]] method can be used in extractors.
     *          {{{
     *          val of: Int => Option[String] = { i =>
     *            if (i == 2) {
     *              Some("matched by an optional function")
     *            } else {
     *              None
     *            }
     *          }
     *
     *          util.Random.nextInt(4) match {
     *            case of.unlift(m) => // Convert an optional function to a pattern
     *              println(m)
     *            case _ =>
     *              println("Not matched")
     *          }
     *          }}}
     */
    def unlift: PartialFunction[A, B] = null
  }

}

/** A function of 1 parameter.
 *
 *  In the following example, the definition of succ is a
 *  shorthand for the anonymous class definition anonfun1:
 *
 *  {{{
 *  object Main extends App {
 *    val succ = (x: Int) => x + 1
 *    val anonfun1 = new Function1[Int, Int] {
 *      def apply(x: Int): Int = x + 1
 *    }
 *    assert(succ(0) == anonfun1(0))
 * }
 *  }}}
 *
 *  Note that the difference between `Function1` and [[scala.PartialFunction]]
 *  is that the latter can specify inputs which it will not handle.
 */
@annotation.implicitNotFound(msg = "No implicit view available from ${T1} => ${R}.")
trait Function1[@specialized(Specializable.Arg) -T1, @specialized(Specializable.Return) +R] extends AnyRef { self =>
  /** Apply the body of this function to the argument.
   *  @return   the result of function application.
   */
  def apply(v1: T1): R

  /** Composes two instances of Function1 in a new Function1, with this function applied last.
   *
   *  @tparam   A   the type to which function `g` can be applied
   *  @param    g   a function A => T1
   *  @return       a new function `f` such that `f(x) == apply(g(x))`
   */
  @annotation.unspecialized def compose[A](g: A => T1): Function1[A , R] = null

  /** Composes two instances of Function1 in a new Function1, with this function applied first.
   *
   *  @tparam   A   the result type of function `g`
   *  @param    g   a function R => A
   *  @return       a new function `f` such that `f(x) == g(apply(x))`
   */
  @annotation.unspecialized def andThen[A](g: R => A): Function1[T1, A] = null

  override def toString(): String = "<function1>"
}


abstract class AbstractPartialFunction[@specialized(Specializable.Arg) -T1, @specialized(Specializable.Return) +R] extends Function1[T1, R] with PartialFunction[T1, R] { self =>
  // this method must be overridden for better performance,
  // for backwards compatibility, fall back to the one inherited from PartialFunction
  // this assumes the old-school partial functions override the apply method, though
  // override def applyOrElse[A1 <: T1, B1 >: R](x: A1, default: A1 => B1): B1 = ???

  // probably okay to make final since classes compiled before have overridden against the old version of AbstractPartialFunction
  // let's not make it final so as not to confuse anyone
  /*final*/ def apply(x: T1): R = ???
}
