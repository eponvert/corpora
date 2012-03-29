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
import multiword.corpus.treebank.constituent.Fixtures._

class BracketSetTestSuite extends FunSuite {

  test("from 1 word, 0 bracket BracketSet to unlabeled") {
    val sentence = Sentence("word")
    val brackets = Set.empty[Bracket[String]]
    val output = BracketSet(sentence, brackets).unlabeled
    val expected = UnlabeledBracketSet(Sentence("word"), Set.empty[Span])
    assert(output === expected)
  }

  test("from 2 word, 0 bracket BracketSet to unlabeled") {
    val sentence = Sentence("word", "up")
    val brackets = Set.empty[Bracket[String]]
    val output = BracketSet(sentence, brackets).unlabeled
    val expected = UnlabeledBracketSet(Sentence("word", "up"), Set.empty[Span])
    assert(output === expected)
  }

  test("from 1 word, 1 bracket BracketSet to unlabeled") {
    val sentence = Sentence("word")
    val brackets = Set(Bracket("S", Span(0, 1), 0))
    val output = BracketSet(sentence, brackets).unlabeled
    val expectedBrackets = Set(Span(0, 1))
    val expected = UnlabeledBracketSet(Sentence("word"), expectedBrackets)
    assert(output.spans === expectedBrackets)
    assert(output === expected)
  }

  test("from 1 word, 2 bracket BracketSet to unlabeled") {
    val sentence = Sentence("word")
    val brackets = Set(
      Bracket("S1", Span(0, 1), 0),
      Bracket("S2", Span(0, 1), 1))
    val output = BracketSet(sentence, brackets).unlabeled
    val expectedBrackets = Set(Span(0, 1))
    val expected = UnlabeledBracketSet(Sentence("word"), expectedBrackets)
    assert(output.spans === expectedBrackets)
    assert(output === expected)
  }

  test("from full 'Pierre Vinken' BracketSet to unlabeled") {
    val output = BracketSet(pvSentence, pvBrackets).unlabeled
  }

  test("from 1 word, 0 bracket BracketSet to tree") {
    val expectedTree = SyntaxTree(Terminal("word"))
    val sentence = Sentence("word")
    val brackets = Set.empty[Bracket[String]]
    val output = BracketSet(sentence, brackets).tree
    assert(output === expectedTree)
  }

  test("from 1 word, 1 bracket BracketSet to tree") {
    val expectedTree = SyntaxTree(LabeledNonTerminal("S", Terminal("word")))
    val sentence = Sentence("word")
    val brackets = Set(Bracket("S", Span(0, 1), 0))
    val output = BracketSet(sentence, brackets).tree
    assert(output === expectedTree)
  }

  test("from 1 word, 2 bracket BracketSet to tree") {
    val expectedTree = SyntaxTree(
      LabeledNonTerminal("S1", LabeledNonTerminal("S2", Terminal("word"))))
    val sentence = Sentence("word")
    val brackets =
      Set(Bracket("S1", Span(0, 1), 0), Bracket("S2", Span(0, 1), 1))
    val output = BracketSet(sentence, brackets).tree
    assert(output === expectedTree)
  }

  test("from 2 word, 1 bracket BracketSet to tree") {
    val expectedTree = SyntaxTree(
      LabeledNonTerminal("S", Terminal("word"), Terminal("up")))
    val sentence = Sentence("word", "up")
    val brackets = Set(Bracket("S", Span(0, 2), 0))
    val output = BracketSet(sentence, brackets).tree
    assert(output === expectedTree)
  }

  test("full 'Pierre Vinken' BracketSet to tree") {
    assert(pvBracketSet.tree === pvSyntaxTree)
  }
}
