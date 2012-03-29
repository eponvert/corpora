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
import multiword.corpus.treebank.constituent.bracket.{ Bracket, BracketSet }
import multiword.corpus.treebank.constituent.Fixtures._

class SyntaxTreeSuite extends FunSuite {

  test("1 terminal 0 non-terminal labeled tree to unlabeled") {
    val output = SyntaxTree(Terminal("word")).unlabeled
    val expected = SyntaxTree(Terminal("word"))
    assert(output === expected)
  }

  test("1 terminal 1 non-terminal labeled tree to unlabeled") {
    val output = SyntaxTree(
      LabeledNonTerminal("S", Terminal("word"))).unlabeled
    val expected = SyntaxTree(UnlabeledNonTerminal(Terminal("word")))
    assert(output === expected)
  }

  test("2 terminal 1 non-terminal labeled tree to unlabeled") {
    val output = SyntaxTree(
      LabeledNonTerminal("S", Terminal("word"), Terminal("up"))).unlabeled
    val expected =
      SyntaxTree(UnlabeledNonTerminal(Terminal("word"), Terminal("up")))
    assert(output === expected)
  }

  test("full 'Pierre Vinken' syntax tree to unlabeled") {
    assert(pvSyntaxTree.unlabeled === pvUnlabeledSyntaxTree)
  }

  test("1 termnial 0 non-terminal syntax tree to bracket set") {
    val output = SyntaxTree(Terminal("word")).bracketSet
    val expectedSentence = Sentence("word")
    val expectedBrackets = Set.empty[Bracket[String]]
    val expectedBracketSet = BracketSet(expectedSentence, expectedBrackets)
    assert(output.sentence === expectedSentence)
    assert(output.brackets === expectedBrackets)
    assert(output === expectedBracketSet)
  }

  test("1 terminal 1 non-terminal syntax tree to bracket set") {
    val output =
      SyntaxTree(LabeledNonTerminal("S", Terminal("word"))).bracketSet
    val expectedSentence = Sentence("word")
    val expectedBrackets = Set(Bracket("S", Span(0, 1), 0))
    val expectedBracketSet = BracketSet(expectedSentence, expectedBrackets)
    assert(output.sentence === expectedSentence)
    assert(output.brackets === expectedBrackets)
    assert(output === expectedBracketSet)
  }

  test("1 terminal 2 non-terminal syntax tree to bracket set") {
    val output = SyntaxTree(
      LabeledNonTerminal("S1", LabeledNonTerminal("S2", Terminal("word"))))
      .bracketSet
    val expectedSentence = Sentence("word")
    val expectedBrackets =
      Set(Bracket("S1", Span(0, 1), 0), Bracket("S2", Span(0, 1), 1))
    val expectedBracketSet = BracketSet(expectedSentence, expectedBrackets)
    assert(output.sentence === expectedSentence)
    assert(output.brackets === expectedBrackets)
    assert(output === expectedBracketSet)
  }

  test("2 terminal 1 non-terminal syntax tree to bracket set") {
    val output = SyntaxTree(
      LabeledNonTerminal("S", Terminal("word"), Terminal("up"))).bracketSet
    val expectedSentence = Sentence("word", "up")
    val expectedBrackets = Set(Bracket("S", Span(0, 2), 0))
    val expectedBracketSet = BracketSet(expectedSentence, expectedBrackets)
    assert(output.sentence === expectedSentence)
    assert(output.brackets === expectedBrackets)
    assert(output === expectedBracketSet)
  }

  test("full 'Pierre Vinken' tree to bracket set") {
    val output = pvSyntaxTree.bracketSet
    assert(output.sentence === pvSentence)
    assert(output.brackets === pvBrackets)
    assert(output === pvBracketSet)
  }
}
