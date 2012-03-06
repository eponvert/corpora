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

import org.scalatest.FunSuite
import multiword.corpus.core._
import TreeNode._
import multiword.corpus.treebank.constituent.bracket.Bracket
import multiword.corpus.treebank.constituent.bracket.BracketSet

class SyntaxTreeSuite extends FunSuite {
  test("single word sentence tree converts into a bracket set") {
    val tree = SyntaxTree(TreeNode("word"))
    val sentence = Sentence(Seq(Token("word")))
    val bracketSet = BracketSet(sentence, Seq.empty[Bracket[String]])
    assert(tree.toBracketSet === bracketSet)
  }

  test("single word syntax tree with root converts into a bracket set") {
    val tree = SyntaxTree(TreeNode("S", TreeNode("word")))
    val sentence = Sentence(Seq(Token("word")))
    val bracketSet = BracketSet(sentence, Seq(Bracket("S", Span(0, 0), 0)))
    assert(tree.toBracketSet === bracketSet)
  }

  test("two word syntax tree converts into a bracket set") {
    val tree = SyntaxTree(TreeNode("S", TreeNode("word"), TreeNode("up")))
    val sentence = Sentence(Seq(Token("word")))
    val bracketSet = BracketSet(sentence, Seq(Bracket("S", Span(0, 1), 0)))
    assert(tree.toBracketSet === bracketSet)
  }

  test("'Pierre Vinken' syntax tree converts into a bracket set") {
    val tree = SyntaxTree(
      TreeNode("S",
        TreeNode("NP-SBJ",
          TreeNode("NP",
            TreeNode("NNP", TreeNode("Pierre")),
            TreeNode("NNP", TreeNode("Vinken"))),
          TreeNode(",", TreeNode(",")),
          TreeNode("ADJP",
            TreeNode("NP",
              TreeNode("CD", TreeNode("61")),
              TreeNode("NNS", TreeNode("years"))),
            TreeNode("JJ", TreeNode("old"))),
          TreeNode(",", TreeNode(","))),
        TreeNode("VP", TreeNode("MD", TreeNode("will")),
          TreeNode("VP", TreeNode("VB", TreeNode("join")),
            TreeNode("NP",
              TreeNode("DT", TreeNode("the")),
              TreeNode("NN", TreeNode("board"))),
            TreeNode("PP-CLR", TreeNode("IN", TreeNode("as")),
              TreeNode("NP",
                TreeNode("DT", TreeNode("a")),
                TreeNode("JJ", TreeNode("nonexecutive")),
                TreeNode("NN", TreeNode("director")))),
            TreeNode("NP-TMP",
              TreeNode("NNP", TreeNode("Nov.")),
              TreeNode("CD", TreeNode("29"))))),
        TreeNode(".", TreeNode("."))))

    val text = "Pierre Vinken , 61 years old , will join the board as a " + 
        "nonexecutive director Nov. 29 ."
    val sentence = Sentence(text split("\\s+") map { Token(_) })
    val brackets = Seq(Bracket("S", Span(0, 17), 0),
                       Bracket("NP-SBJ", Span(0, 5), 1),
                       Bracket("NP", Span(0, 1), 2),
                       Bracket("NNP", Span(0, 0), 3),
                       Bracket("NNP", Span(1, 1), 4),
                       Bracket(",", Span(2, 2), 5),
                       Bracket("ADJP", Span(3, 5), 6),
                       Bracket("NP", Span(3, 4), 7),
                       Bracket("CD", Span(3, 3), 8),
                       Bracket("NNS", Span(4, 4), 9),
                       Bracket("JJ", Span(5, 5), 10),
                       Bracket(",", Span(6, 6), 11),
                       Bracket("VP", Span(7, 16), 12),
                       Bracket("MD", Span(7, 7), 13),
                       Bracket("VP", Span(8, 16), 14),
                       Bracket("VB", Span(8, 8), 15),
                       Bracket("NP", Span(9, 10), 16), 
                       Bracket("DT", Span(9, 9), 17),
                       Bracket("NN", Span(10, 10), 18),
                       Bracket("PP-CLR", Span(11, 14), 19),
                       Bracket("IN", Span(11, 11), 20),
                       Bracket("NP", Span(12, 14), 21),
                       Bracket("DT", Span(12, 12), 22),
                       Bracket("JJ", Span(13, 13), 23),
                       Bracket("NN", Span(14, 14), 24),
                       Bracket("NP-TMP", Span(15, 16), 25),
                       Bracket("NNP", Span(15, 15), 26),
                       Bracket("CD", Span(16, 16), 27),
                       Bracket(".", Span(17, 17), 28))
    val bracketSet = BracketSet(sentence, brackets)

    assert(tree.toBracketSet === bracketSet)
  }
}

