package org.klesun.lang

import java.io.{PrintWriter, StringWriter}
import java.util

import com.intellij.psi.PsiElement
import org.klesun.deep_js_completion.contexts.Debug

import scala.collection.JavaConverters._
import scala.collection.{AbstractIterable, GenTraversableOnce}
import scala.reflect.{ClassTag, classTag}

class ExplicitNull extends Throwable("not exception - used in normal program flow", null, false, false) {
}

/** provides some core functions needed for IDEA plugin development */
object DeepJsLang {

	// it would be nice to tell compiler somehow that T
	// is subclass of value arg... I could not find the way
	def cast[T: ClassTag](value: Any): Option[T] = {
		value match {
			/** @see https://stackoverflow.com/a/21640639/2750743 */
			case matched: T if classTag[T].runtimeClass.isInstance(matched) => Some(matched)
			case _ => None
		}
	}

	// looks like it's to complex for Scala compiler so I guess I won't use it after all...
	//  def toCast[B, A <: PsiElement]: A => Option[B] = {
	//    obj => cast[B](obj)
	//  }

	/**
	 * you can use it to println() some text to console without breaking Option chain:
	 * Option(...)
	 * .map(...)
	 * .filter(val => Lang.log("some text " + val))
	 * .map(...)
	 */
	def log(msg: String) = {
		println(msg)
		true
	}

	def substr(str: String, startIndexArg: Int, endIndexArg: Int): String = {
		var startIndex = startIndexArg
		var endIndex = endIndexArg
		if (startIndex < 0) {
			startIndex = str.length() + startIndex
		}
		if (endIndex < 0) {
			endIndex = str.length() + endIndex
		}

		if (str.length() > 0 && startIndex < endIndex) {
			str.substring(
				Math.max(0, startIndex),
				Math.min(str.length(), endIndex)
			)
		} else {
			""
		}
	}

	def substr(str: String, startIndex: Int): String = {
		substr(str, startIndex, str.length())
	}

	def all[T](opts: GenTraversableOnce[Option[T]]): Option[Iterable[T]] = {
		val values = new util.ArrayList[T]()
		var iter = opts.itr()
		var break = false
		while (!break && iter.hasNext) {
			val opt = iter.next()
			if (opt.nonEmpty) {
				values.add(opt.get)
			} else {
				break = true
			}
		}
		if (break) {
			None
		} else {
			Some(values.asScala)
		}
	}

	def frsOpt[T](opts: Option[T]*): Option[T] = {
		for (opt <- opts) {
			if (opt.nonEmpty) {
				return opt
			}
		}
		None
	}

	def singleLine(text: String, length: Int): String = {
		val lines = text.split("\n").map(l => l.trim)
		substr(lines.mkString(" "), 0, length)
	}

	def findParent[T <: PsiElement : ClassTag](psi: PsiElement): Option[T] = {
		var parent = psi.getParent
		var matching: Option[T] = None
		while (parent != null && matching.isEmpty) {
			matching = cast[T](parent)
			parent = parent.getParent
		}
		matching
	}

	//  def findChildrenImpl[T <: PsiElement : ClassTag](parent: PsiElement, occurrences: mutable.HashSet[PsiElement]): List[T] = {
	//    occurrences.add(parent)
	//    parent.getChildren
	//      .filter(c => !occurrences.contains(c))
	//      .flatMap(c => findChildrenImpl[T](c, occurrences))
	//      .++(List(parent).flatMap(cast[T](_)))
	//      .itr
	//  }
	//
	//  def findChildren[T <: PsiElement : ClassTag](parent: PsiElement): List[T] = {
	//    findChildrenImpl(parent, new mutable.HashSet[PsiElement]())
	//  }

	def findChildren[T <: PsiElement : ClassTag](parent: PsiElement): It[T] = {
		parent.getChildren
			.flatMap(c => findChildren[T](c))
			.++(List(parent).flatMap(cast[T](_)))
			.itr
	}

	def getStackTrace(exc: Throwable): String = {
		val sw = new StringWriter
		val pw = new PrintWriter(sw)
		exc.printStackTrace(pw)
		sw.toString
	}

	def getStackTrace(): String = getStackTrace(new Exception)

	implicit class BetterArray[T](base: Array[T]) {
		def itr(): It[T] = {
			new It[T](base)
		}

		def mem(): MemIt[T] = {
			new MemIt[T](base)
		}
	}

	implicit class BetterGenTraversableOnce[T](base: GenTraversableOnce[T]) {
		def itr(): It[T] = {
			base match {
				case ts: It[T] => ts
				case _ => new It[T](base)
			}
		}

		def mem(): MemIt[T] = {
			new MemIt[T](base)
		}

		def ++(other: GenTraversableOnce[T]): It[T] = {
			itr() ++ other
		}
	}

	implicit class BetterOption[T](base: Option[T]) {
		def itr(): It[T] = {
			new It[T](base)
		}

		def mem(): MemIt[T] = {
			new MemIt[T](base)
		}

		/** stands for "Unwrap Explicit Null Check" */
		@throws(classOf[ExplicitNull])
		def unl(): T = {
			notNone(base)
		}
	}

	def onDemand[T](supplier: () => T): () => T = {
		var valueOpt: Option[T] = None
		() => {
			if (valueOpt.isEmpty) {
				valueOpt = Some(supplier())
			}
			valueOpt.get
		}
	}

	class MemIt[T](values: GenTraversableOnce[T]) extends AbstractIterable[T] {
		val memoizedValues = new util.ArrayList[T]()
		var complete = false
		val sourceBle = onDemand(() => values.toIterator)
		// make sure nobody will start iterating over this
		// src before first iteration finished, since it
		// could cause an infinite recursion otherwise
		private var isNexting = false

		/** for debug mostly */
		def fst(): Option[T] = {
			val itr = this.itr()
			if (itr.hasNext) {
				Some(itr.next())
			} else {
				None
			}
		}

		override def iterator: Iterator[T] = {
			val source = sourceBle()
			var idx = 0
			new Iterator[T] {
				def hasNext(): Boolean = {
					if (isNexting) {
						false
					} else {
						isNexting = true
						val has = idx < memoizedValues.size() || (!complete && source.hasNext)
						if (!has) {
							complete = true
						}
						isNexting = false
						has
					}
				}

				def next(): T = {
					if (isNexting) {
						throw new RuntimeException("Tried to next MemIt again when still not done nexting")
					}
					isNexting = true
					if (idx >= memoizedValues.size()) {
						memoizedValues.add(source.next())
					}
					val next = memoizedValues.get(idx)
					idx = memoizedValues.size()
					isNexting = false
					next
				}
			}.itr()
		}

		override def toString(): String = {
			val infix = if (complete) "" else ", ..."
			"MemIt(" + memoizedValues.asScala.mkString(",") + infix + ")"
		}
	}

	/**
	 * unlike built-in .toIterator(), this one throws
	 * exception if you try to iterate over same iterator again
	 * this is important because it's way to easy to miss a mistake otherwise
	 */
	class It[T](values: GenTraversableOnce[T]) extends Iterator[T] {
		private val src = values.toIterator
		private var ended = false
		private var allowEndHasNextFlag = false
		private var hadAny = false
		private var disposed = false

		/** @debug */
		val createdAt = if (Debug.DEBUG_DISPOSED_ITER) Some(new RuntimeException("created here")) else None
		var disposedAt: Option[Exception] = None

		override def hasNext: Boolean = {
			if (ended) {
				if (allowEndHasNextFlag || !hadAny) {
					false
				} else {
					val exc = new RuntimeException("Tried to reuse disposed iterator")
					if (disposedAt.nonEmpty) {
						exc.initCause(disposedAt.get)
						exc.printStackTrace()
						disposedAt.get.printStackTrace()
						createdAt.get.printStackTrace()
					}
					throw exc
				}
			} else if (src.hasNext) {
				hadAny = true
				true
			} else {
				if (createdAt.nonEmpty) {
					disposedAt = Some(new Exception("disposed here", createdAt.get))
				}
				ended = true
				false
			}
		}

		def dispose(): Unit = {
			if (disposed) {
				throw new RuntimeException("Tried to reuse disposed iterator ", disposedAt.orNull)
			}
			if (createdAt.nonEmpty) {
				disposedAt = Some(new Exception("disposed here", createdAt.get))
			}
			disposed = true
		}

		override def next(): T = src.next()

		override def filter(f: T => Boolean): It[T] = {
			dispose()
			new It(values.toIterator.filter(f))
		}

		override def map[Tnew](f: T => Tnew): It[Tnew] = {
			dispose()
			new It(values.toIterator.map(f)).allowEndHasNext(allowEndHasNextFlag)
		}

		override def flatMap[Tnew](f: T => GenTraversableOnce[Tnew]): It[Tnew] = {
			dispose()
			new It(values.toIterator.flatMap(f))
		}

		def ++(other: GenTraversableOnce[T]): It[T] = {
			dispose()
			new It(values.toIterator ++ other)
		}

		def lift(n: Int): Option[T] = {
			dispose()
			var i = 0
			while (i < n && hasNext) {
				next()
				i = i + 1
			}
			if (hasNext) Some(next()) else None
		}

		def unq(): It[T] = {
			val met = new util.HashSet[T]
			filter(el => {
				if (met.contains(el)) {
					false
				} else {
					met.add(el)
					true
				}
			})
		}

		def unq[Thash](getHash: (T) => Thash): It[T] = {
			val met = new util.HashSet[Thash]()
			filter(el => {
				val hash = getHash(el)
				if (met.contains(hash)) {
					false
				} else {
					met.add(hash)
					true
				}
			})
		}

		/**
		 * should not be needed in deep-js code, but deep-assoc, which uses it, has
		 * MemIt implementation that may ping hasNext() multiple times for same value
		 */
		def allowEndHasNext(flag: Boolean) = {
			allowEndHasNextFlag = flag
			this
		}
	}

	def itr[T](values: GenTraversableOnce[T]): It[T] = {
		new It(values)
	}

	def cnc[T](tors: GenTraversableOnce[T]*): It[T] = {
		itr(tors).flatMap(tor => tor)
	}

	/** "Nullable ITerator" - because I'm sick of writing Option(value).itr.flatMap(...) */
	def nit[T](value: T): It[T] = {
		itr(Option(value))
	}

	/**
	 * I am sick of if `opt.isEmpty() return else doStuff(opt.get())` checks - notNull() function is intended to
	 * assert not null value in one line and throw a special no-stack-trace exception to be caught by Option.map()
	 */
	@throws(classOf[ExplicitNull])
	def notNull[T](value: T): T = {
		if (value == null) {
			throw new ExplicitNull
		} else {
			value
		}
	}

	@throws(classOf[ExplicitNull])
	def notNone[T](opt: Option[T]): T = {
		if (opt.isEmpty) {
			throw new ExplicitNull
		} else {
			opt.get
		}
	}

	/** "FaLlBack" - return first non-empty iterator from the passed values */
	def frs[T](suppliers: GenTraversableOnce[T]*): It[T] = {
		itr(suppliers)
			.map(trav => trav.itr())
			.filter(v => v.hasNext)
			.lift(0).itr()
			.flatMap(tor => tor)
	}

	def getPrevSiblings(psi: PsiElement): GenTraversableOnce[PsiElement] = {
		var prev = psi.getPrevSibling
		new Iterator[PsiElement] {
			override def hasNext: Boolean = prev != null

			override def next(): PsiElement = {
				val tmp = prev
				prev = prev.getPrevSibling
				tmp
			}
		}
	}
}
