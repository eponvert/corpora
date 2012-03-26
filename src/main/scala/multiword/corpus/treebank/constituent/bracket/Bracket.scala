/* Copyright 2012 Elias Ponvert
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package multiword.corpus.treebank.constituent.bracket

import multiword.corpus.core._
import multiword.corpus.treebank._
import multiword.corpus.treebank.constituent.tree._
import scala.collection.mutable

/**
 * A constituent associated with span of tokens in a sentence; constituents
 * must not overlap, and hence they can be partially ordered by containment;
 * in the event that two constituents are associated with the same span, one
 * is still considered "higher" in the tree; thus a constituent has an index
 * associated with it
 * @param label the label of the bracket
 * @param start the token index of the first token in the bracket
 * @param end the token index of the last token in the bracket
 * @param dfsIndex means "depth-first search index" -- this number does not
 *   necessarily have intrinsic value, but it must be higher than its parent's
 *   `dfsIndex`
 * @tparam L the type of the label
 */
case class Bracket[L](val label: L, val span: Span, val dfsIndex: Int)
  extends LinguisticEntity {
  /** The string to print before a bracket when converting to String */
  lazy val bracketPrefix = "(" + label

  /** The string to print after a bracket when converting to String */
  val bracketPostfix = ")"
}

/**
 * A set of bracket annotations for a sentence
 * @param length the number of tokens in the sentence
 * @param braks the set of brackets in the sentence annotations
 * @tparam N the type of the bracket labels
 * @tparam T the type of the underlying class of terms
 */
case class BracketSet[N, T](
  val sentence: Sentence[T],
  val brackets: Set[Bracket[N]])
  extends Annotation[Sentence[T]] {

  import BracketSet._

  // firstInd = map from each token index to the brackets starting at that index
  // lastInd = map from each token index to the brackets ending at that index
  private val (firstInd, lastInd) = {
    val _firstInd = mutableIndexSeq[N](sentence.length)
    val _lastInd = mutableIndexSeq[N](sentence.length)
    brackets.filter(_.span.length > 0).foreach(bracket => {
      _firstInd(bracket.span.start) += bracket
      _lastInd(bracket.span.end) += bracket
    })
    (immutableSeq(_firstInd), immutableSeq(_lastInd))
  }

  lazy val tree: SyntaxTree[N, T] = {
    null
  }
}

object BracketSet {

  private def mutableIndexSeq[L](n: Int) =
    (for (_ <- 0 to n) yield mutable.Buffer[Bracket[L]]()).toIndexedSeq

  /**
   * Creates an array of immutable sequences of brackets sorted by dfsIndex
   * from an array of mutable
   */
  private def immutableSeq[L](s: Seq[mutable.Seq[Bracket[L]]]) =
    s.map(_.sortBy(_.dfsIndex).toIndexedSeq)
}
