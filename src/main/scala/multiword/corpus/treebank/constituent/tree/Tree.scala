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

package multiword.corpus.treebank.constituent.tree

import multiword.corpus.core._
import scala.collection.mutable
import multiword.corpus.treebank.constituent._
import multiword.corpus.treebank.constituent.bracket._

class SyntaxTreeException extends ConstituentException

class NotWellFormedSyntaxTree extends SyntaxTreeException

case class SyntaxTree[N, T](val root: TreeNode[N, T])
  extends Annotation[Sentence[T]] {

  /** Convert this tree structure to a {@link BracketSet} structure */
  lazy val bracketSet: BracketSet[N, T] = {
    var tokenIndex = 0
    var dfsIndex = 0
    val startIndices = mutable.Stack[Int]()
    val labels = mutable.Stack[N]()
    val dfsIndices = mutable.Stack[Int]()
    val nodes = mutable.Stack[Option[TreeNode[N, T]]](Some(root))
    val sentenceBuffer = mutable.Buffer[T]()
    val brackets = mutable.Buffer[Bracket[N]]()
    while (!nodes.isEmpty) {
      nodes.pop match {
        case Some(node) => {
          node match {
            case Terminal(symbol) => {
              sentenceBuffer += symbol
              tokenIndex += 1
            }
            case NonTerminal(label, daughters) => {
              labels push label
              dfsIndices push dfsIndex
              startIndices push tokenIndex
              dfsIndex += 1
              nodes push None
              daughters.toSeq.reverse.foreach(d => {
                nodes push Some(d)
              })
            }
          }
        }
        case None => {
          val label = labels.pop
          val start = startIndices.pop
          val end = tokenIndex
          val dfsIndex = dfsIndices.pop
          brackets += Bracket(label, Span(start, end), dfsIndex)
        }
      }
    }
    BracketSet(Sentence(sentenceBuffer map { Token(_) } toIndexedSeq),
      brackets.toSet)
  }
}

/**
 * General class of the tree data structure
 * @tparam N the node or non-terminal label type
 * @tparam T the leaf or terminal symbol type
 */
abstract class TreeNode[N, T] {

  /** Make an immutable copy of self */
  def immutable: TreeNode[N, T]
}

/**
 * A non-terminal node in an abstract syntax tree
 * @param label the
 */
case class NonTerminal[N, T](
  val label: N,
  val daughters: Iterable[TreeNode[N, T]]) extends TreeNode[N, T] {

  override def immutable =
    NonTerminal(label, (daughters.map(_.immutable)).toIndexedSeq)
}

case class Terminal[N, T](val symbol: T) extends TreeNode[N, T] {
  override def immutable = this
}

object TreeNode {
  def apply[N](symbol: N, daughters: TreeNode[N, N]*) =
    if (daughters.isEmpty) new Terminal[N, N](symbol)
    else new NonTerminal(symbol, daughters)
}
