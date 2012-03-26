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

import org.scalatest.FunSuite
import multiword.corpus.core._
import multiword.corpus.treebank.constituent.tree._

class BracketSetTestSuite extends FunSuite {

  test("from 1 word, 0 bracket BracketSet to tree") {
    val expectedTree = SyntaxTree(TreeNode("word"))
    val sentence = Sentence("word")
    val brackets = Set.empty[Bracket[String]]
    val output = BracketSet(sentence, brackets).tree
    assert(output === expectedTree)
  }

  test("from 1 word, 1 bracket BracketSet to tree") {
    val expectedTree = SyntaxTree(TreeNode("S", TreeNode("word")))
    val sentence = Sentence("word")
    val brackets = Set(Bracket("S", Span(0, 1), 0))
    val output = BracketSet(sentence, brackets).tree
    assert(output === expectedTree)
  }

  test("from 1 word, 2 bracket BracketSet to tree") {
    val expectedTree =
      SyntaxTree(TreeNode("S1", TreeNode("S2", TreeNode("word"))))
    val sentence = Sentence("word")
    val brackets =
      Set(Bracket("S1", Span(0, 1), 0), Bracket("S2", Span(0, 1), 1))
    val output = BracketSet(sentence, brackets).tree
    assert(output === expectedTree)
  }

  test("from 2 word, 1 bracket BracketSet to tree") {
    val expectedTree =
      SyntaxTree(TreeNode("S", TreeNode("word"), TreeNode("up")))
    val sentence = Sentence("word", "up")
    val brackets = Set(Bracket("S", Span(0, 2), 0))
    val output = BracketSet(sentence, brackets).tree
    assert(output === expectedTree)
  }

  test("full 'Pierre Vinken' BracketSet to tree") {
    val expectedTree = SyntaxTree(
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
    val sentence = Sentence(text.split("\\s+"):_*)
    val brackets = Set(
      Bracket("S", Span(0, 18), 0),
      Bracket("NP-SBJ", Span(0, 7), 1),
      Bracket("NP", Span(0, 2), 2),
      Bracket("NNP", Span(0, 1), 3),
      Bracket("NNP", Span(1, 2), 4),
      Bracket(",", Span(2, 3), 5),
      Bracket("ADJP", Span(3, 6), 6),
      Bracket("NP", Span(3, 5), 7),
      Bracket("CD", Span(3, 4), 8),
      Bracket("NNS", Span(4, 5), 9),
      Bracket("JJ", Span(5, 6), 10),
      Bracket(",", Span(6, 7), 11),
      Bracket("VP", Span(7, 17), 12),
      Bracket("MD", Span(7, 8), 13),
      Bracket("VP", Span(8, 17), 14),
      Bracket("VB", Span(8, 9), 15),
      Bracket("NP", Span(9, 11), 16),
      Bracket("DT", Span(9, 10), 17),
      Bracket("NN", Span(10, 11), 18),
      Bracket("PP-CLR", Span(11, 15), 19),
      Bracket("IN", Span(11, 12), 20),
      Bracket("NP", Span(12, 15), 21),
      Bracket("DT", Span(12, 13), 22),
      Bracket("JJ", Span(13, 14), 23),
      Bracket("NN", Span(14, 15), 24),
      Bracket("NP-TMP", Span(15, 17), 25),
      Bracket("NNP", Span(15, 16), 26),
      Bracket("CD", Span(16, 17), 27),
      Bracket(".", Span(17, 18), 28))
    val output = BracketSet(sentence, brackets).tree
    assert(output === expectedTree)
  }
}
