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

package multiword.corpus.treebank.constituent

import multiword.corpus.core._
import multiword.corpus.treebank.constituent.bracket._
import multiword.corpus.treebank.constituent.tree._

object Fixtures {
  /** The text of the first sentence in the Penn Treebank */
  lazy val pvText =
    "Pierre Vinken , 61 years old , will join the board as a nonexecutive " +
      "director Nov. 29 ."

  /**
   * A [[multiword.corpus.core.Sentence]] structure for the first sentence in the
   * Penn Treebank
   */
  lazy val pvSentence = Sentence(pvText.split("\\s+"): _*)

  /**
   * The set of [[multiword.corpus.treebank.constituent.bracket.Bracket]]s
   * for the first sentence in the Penn Treebank.
   */
  lazy val pvBrackets = Set(
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

  lazy val pvUnlabeledBrackets = Set(
    Span(0, 18),
    Span(0, 7),
    Span(0, 2),
    Span(3, 6),
    Span(3, 5),
    Span(7, 17),
    Span(8, 17),
    Span(9, 11),
    Span(11, 15),
    Span(12, 15),
    Span(15, 17))

  /**
   * The [[multiword.corpus.treebank.constituent.bracket.BracketSet]] for the
   * first sentence in the Penn Treebank
   */
  lazy val pvBracketSet = BracketSet(pvSentence, pvBrackets)

  /** The labeled syntax tree for the first sentence of the Penn Treebank */
  lazy val pvSyntaxTree = SyntaxTree(
    LabeledNonTerminal("S",
      LabeledNonTerminal("NP-SBJ",
        LabeledNonTerminal("NP",
          LabeledNonTerminal("NNP", Terminal("Pierre")),
          LabeledNonTerminal("NNP", Terminal("Vinken"))),
        LabeledNonTerminal(",", Terminal(",")),
        LabeledNonTerminal("ADJP",
          LabeledNonTerminal("NP",
            LabeledNonTerminal("CD", Terminal("61")),
            LabeledNonTerminal("NNS", Terminal("years"))),
          LabeledNonTerminal("JJ", Terminal("old"))),
        LabeledNonTerminal(",", Terminal(","))),
      LabeledNonTerminal("VP", LabeledNonTerminal("MD", Terminal("will")),
        LabeledNonTerminal("VP", LabeledNonTerminal("VB", Terminal("join")),
          LabeledNonTerminal("NP",
            LabeledNonTerminal("DT", Terminal("the")),
            LabeledNonTerminal("NN", Terminal("board"))),
          LabeledNonTerminal("PP-CLR", LabeledNonTerminal("IN", Terminal("as")),
            LabeledNonTerminal("NP",
              LabeledNonTerminal("DT", Terminal("a")),
              LabeledNonTerminal("JJ", Terminal("nonexecutive")),
              LabeledNonTerminal("NN", Terminal("director")))),
          LabeledNonTerminal("NP-TMP",
            LabeledNonTerminal("NNP", Terminal("Nov.")),
            LabeledNonTerminal("CD", Terminal("29"))))),
      LabeledNonTerminal(".", Terminal("."))))

  /** The unlabeled syntax tree for the first sentence in the Penn Treebank */
  lazy val pvUnlabeledSyntaxTree = SyntaxTree(
    UnlabeledNonTerminal(
      UnlabeledNonTerminal(
        UnlabeledNonTerminal(Terminal("Pierre"), Terminal("Vinken")),
        Terminal(","),
        UnlabeledNonTerminal(
          UnlabeledNonTerminal(Terminal("61"), Terminal("years")),
          Terminal("old")),
        Terminal(",")),
      UnlabeledNonTerminal(
        Terminal("will"),
        UnlabeledNonTerminal(
          Terminal("join"),
          UnlabeledNonTerminal(Terminal("the"), Terminal("board")),
          UnlabeledNonTerminal(
            Terminal("as"),
            UnlabeledNonTerminal(
              Terminal("a"), Terminal("nonexecutive"), Terminal("director"))),
          UnlabeledNonTerminal(Terminal("Nov."), Terminal("29")))),
      Terminal(".")))
}
