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

class SyntaxTree[N, T](val root:TreeNode[N, T]) extends Annotation[Sentence[T]] {

  /** Convert this tree structure to a {@link BracketSet} structure */
  def toBracketSet:BracketSet[N,T] = {
    var tokenIndex = 0
    var dfsIndex = 0
    val startIndices = mutable.Stack[Int]()
    val labels = mutable.Stack[N]()
    val dfsIndices = mutable.Stack[Int]()
    val nodes = mutable.Stack(Some(root), None)
    val sentence = mutable.Seq[T]()
    var nonTerminals = mutable.Seq[NonTerminal[N, T]]()
    val brackets = mutable.Seq[Bracket[N]]()
    while (!nodes.isEmpty) {
      nodes.pop match {
        case Some(node) => {
          node match {
            case Terminal(symbol) => {
              sentence :+ symbol
              tokenIndex += 1
            }
            case NonTerminal(label, daughters) => {
              labels push label
              dfsIndices push dfsIndex
              dfsIndex += 1
              daughters.toSeq.reverse.foreach(d => { 
                nodes push Some(d)
                nodes push None })
            }
          }
        }
        case None => {
          brackets :+ 
            Bracket(labels.pop, startIndices.pop, tokenIndex, dfsIndices.pop)
        }
      }
    }
    BracketSet(Sentence(sentence map { Token(_) }), brackets)
  }
}

object SyntaxTree {
  def apply[N,T](root:TreeNode[N,T]) = new SyntaxTree(root) 
}

/** General class of the tree data structure
 * @tparam N the node or non-terminal label type
 * @tparam T the leaf or terminal symbol type
 */
abstract class TreeNode[N,T]

/** A non-terminal node in an abstract syntax tree
 * @param label the  */
case class NonTerminal[N,T](
    val label:N,
    val daughters:Iterable[TreeNode[N,T]]) extends TreeNode[N,T]

/*
object NonTerminal {
  def apply[N,T](label:N, daughters:TreeNode[N,T]*) = 
    new NonTerminal(label, daughters)
}
*/

case class Terminal[N,T](val symbol:T) extends TreeNode[N,T]

/*
object Terminal {
  def apply[N,T](symbol:T) = new Terminal[N,T](symbol)
}
*/

object TreeNode {
  def apply[N](symbol:N, daughters:TreeNode[N,N]*) =
    if (daughters.isEmpty) new Terminal[N,N](symbol) 
    else new NonTerminal(symbol, daughters) 
    
}
