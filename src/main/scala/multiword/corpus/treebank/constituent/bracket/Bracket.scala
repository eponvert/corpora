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

case class UnlabeledBracketSet[T](
  val sentence: Sentence[T], val spans: Set[Span])
  extends Annotation[Sentence[T]] {

  import UnlabeledBracketSet._

  // firstInd = map from each token index to the brackets starting at that index
  // lastInd = map from each token index to the brackets ending at that index
  private lazy val (firstInd, lastInd) = {
    val _firstInd = mutableIndexSeq(sentence.length)
    val _lastInd = mutableIndexSeq(sentence.length)
    spans.filter(_.length > 0).foreach(span => {
      _firstInd(span.start) += span
      _lastInd(span.end) += span
    })
    (immutableSeq(_firstInd), immutableSeq(_lastInd))
  }

  lazy val tree: SyntaxTree[Nothing, T] = {
    val treeNodes = mutable.Stack[TreeNode[Nothing, T]]()
    var node: TreeNode[Nothing, T] = null
    for (index <- 0 until sentence.length) {
      lastInd(index).foreach(_ => treeNodes.pop)
      firstInd(index).foreach(bracket => {
        val daughters = mutable.Buffer[TreeNode[Nothing, T]]()
        node = UnlabeledNonTerminal(daughters: _*)
        if (!treeNodes.isEmpty) {
          treeNodes.top match {
            case UnlabeledNonTerminal(daughters @ _*) => {
              daughters match {
                case m: mutable.Buffer[TreeNode[Nothing, T]] => m += node
              }
            }
          }
        }
        treeNodes.push(node)
      })
      node = Terminal(sentence(index))
      if (!treeNodes.isEmpty) {
        treeNodes.top match {
          case UnlabeledNonTerminal(daughters @ _*) => {
            daughters match {
              case m: mutable.Buffer[TreeNode[Nothing, T]] => m += node
            }
          }
        }
      }
    }
    while (!treeNodes.isEmpty)
      node = treeNodes.pop
    SyntaxTree(node.immutable)
  }
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

  private lazy val smallSentence = sentence.length <= 1

  /**
   * Make an unlabeled version of this bracket set -- the same brackets only
   * without labels; de facto this removes single branch nodes (except
   * pre-terminals). Also, prune pre-terminals (POS) -- the brackets spanning
   * just one term, unless the sentence is only length 1. The dfsIndex
   * is ignored and reset to 0, since the spans are sufficient to distinguish
   * brackets.
   */
  lazy val unlabeled = UnlabeledBracketSet(
    sentence,
    (for (b <- brackets) yield b.span).filter(_.length > 1 || smallSentence))

  // firstInd = map from each token index to the brackets starting at that index
  // lastInd = map from each token index to the brackets ending at that index
  private lazy val (firstInd, lastInd) = {
    val _firstInd = mutableIndexSeq[N](sentence.length)
    val _lastInd = mutableIndexSeq[N](sentence.length)
    brackets.filter(_.span.length > 0).foreach(bracket => {
      _firstInd(bracket.span.start) += bracket
      _lastInd(bracket.span.end) += bracket
    })
    (immutableSeq(_firstInd), immutableSeq(_lastInd))
  }

  lazy val tree: SyntaxTree[N, T] = {
    val treeNodes = mutable.Stack[TreeNode[N, T]]()
    var node: TreeNode[N, T] = null
    for (index <- 0 until sentence.length) {
      lastInd(index).foreach(_ => treeNodes.pop)
      firstInd(index).foreach(bracket => {
        val daughters = mutable.Buffer[TreeNode[N, T]]()
        node = LabeledNonTerminal(bracket.label, daughters: _*)
        if (!treeNodes.isEmpty) {
          treeNodes.top match {
            case LabeledNonTerminal(_, daughters @ _*) => {
              daughters match {
                case m: mutable.Buffer[TreeNode[N, T]] => m += node
              }
            }
          }
        }
        treeNodes.push(node)
      })
      node = Terminal(sentence(index))
      if (!treeNodes.isEmpty) {
        treeNodes.top match {
          case LabeledNonTerminal(_, daughters @ _*) => {
            daughters match {
              case m: mutable.Buffer[TreeNode[N, T]] => m += node
            }
          }
        }
      }
    }
    while (!treeNodes.isEmpty)
      node = treeNodes.pop
    SyntaxTree(node.immutable)
  }
}

object UnlabeledBracketSet {

  private def mutableIndexSeq(n: Int) =
    (for (_ <- 0 to n) yield mutable.Buffer[Span]()).toIndexedSeq

  /**
   * Creates an array of immutable sequences of brackets sorted by dfsIndex
   * from an array of mutable
   */
  private def immutableSeq(s: Seq[mutable.Seq[Span]]) =
    s.map(_.sorted.toIndexedSeq)

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
