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

package multiword.corpus.core

import java.io.Serializable

class CorpusException extends Exception

/** Abstract class of linguistic objects */
abstract class LinguisticEntity extends Serializable

/** Abstract superclass for a term in context
 * @tparam T the type of the underlying format used for terms
 */
abstract class Token[T] extends LinguisticEntity {

  /** Return the term string associated with this token */
  def term:T
}

/** Simple implementation of a token, wrapping a term with no further
 * information
 * @param term this token's term
 * @tparam T the type of the underlying format used for terms
 */
class SimpleToken[T](val term:T) extends Token[T]

object Token {

  /** Wrap a string in a token object
   * @tparam T the type of the underlying format used for terms
   */
  def apply[T](term:T) = new SimpleToken(term)
}

/** Abstract class for a sentence; a sequence of terms
 * @tparam T the type of the underlying format used for terms
 */
abstract class Sentence[T] extends LinguisticEntity {

  /** Return an iterable over the tokens of the sentence */
  def tokens:Iterable[Token[T]]

  /** Return the number of tokens in the sentence */
  def length:Int
}

/** Simple implemenation of a sentence consisting of a sequence of tokens
 * @param tokens the sequence of tokens for this sentence
 * @tparam T the type of the underlying format used for terms
 */
class SimpleSentence[T](val tokens:Seq[Token[T]]) extends Sentence[T] {
  override def length = tokens.length
}

object Sentence {

  /** Create a simple sentence from a sequence of tokens
   * @param tokens the sequence of tokens for this sentence
   * @tparam T the type of the underlying format used for terms
   */
  def apply[T](tokens:Iterable[Token[T]]) = new SimpleSentence(tokens.toSeq)
}

/** A span of entities, such as tokens */
class Span(val start:Int, val end:Int) extends LinguisticEntity {

  /** Return the length of this span */
  lazy val length = end - start
}

object Span {
  def apply(start:Int, end:Int) = new Span(start, end) 
}

/** Abstract superclass for documents */
abstract class Document extends LinguisticEntity

/** Linguistic annotation: either provided by human subject-matter experts, or
 * provided by machine annotators
 * @tparam L the type of linguistic entity the annotation is relevant to
 */
trait Annotation[L <: LinguisticEntity]

/** Simple label-annotations for linguistic entities; can apply to
 * @tparam L the type of linguistic entity the annotation is relevant to
 * @tparam A the type used for labels
 */
class Labeled[L <: LinguisticEntity, A](val entity:L, val label:A)
extends Annotation[L]

object Labeled {
  /** Create a simple annotation for the linguistic entity with the label
   * provided
   * @param entity the liguistic entity being annotated
   * @param label the label annotating the entity
   * @tparam L the type of linguistic entity the annotation is relevant to
   * @tparam A the type used for labels
   */
  def apply[L <: LinguisticEntity, A](entity:L, label:A) =
    new Labeled(entity, label)
}