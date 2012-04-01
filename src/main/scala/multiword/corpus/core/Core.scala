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

/**
 * Abstract superclass for a term in context
 * @tparam T the type of the underlying format used for terms
 */
abstract class Token[T] extends LinguisticEntity {

  /** Return the term string associated with this token */
  def term: T
}

/**
 * Simple implementation of a token, wrapping a term with no further
 * information
 * @param term this token's term
 * @tparam T the type of the underlying format used for terms
 */
case class SimpleToken[T](val term: T) extends Token[T]

object Token {

  /**
   * Wrap a string in a token object
   * @tparam T the type of the underlying format used for terms
   */
  def apply[T](term: T) = SimpleToken(term)
}

/**
 * Abstract class for a sentence; a sequence of terms
 * @param tokens the sequence of tokens in this sentence
 * @tparam T the type of the underlying format used for terms
 */
case class Sentence[T](val tokens: IndexedSeq[Token[T]])
  extends LinguisticEntity {

  /** Return the number of tokens in the sentence */
  lazy val length = tokens.length

  /**
   * Return the ith term in the sentence
   * @param i the index of the token to return
   */
  def apply(i: Int) = tokens(i).term
}

object Sentence {
  def apply[T](tokens: T*) = new Sentence(tokens map { Token(_) } toIndexedSeq)
}

class UncomparableSpans extends CorpusException

/** A span of entities, such as tokens */
case class Span(val start: Int, val end: Int)
  extends LinguisticEntity with Ordered[Span] {

  /** Return the length of this span */
  lazy val length = end - start

  override def compare(that: Span) =
    if (this == that)
      0

    // if that is a daughter of this, then this is GREATER than that
    else if (this.start <= that.start && that.end <= this.end)
      1

    else if (that.start <= this.start && this.end <= that.end)
      -1

    else
      throw new UncomparableSpans
}

/** Abstract superclass for documents */
abstract class Document extends LinguisticEntity

/**
 * Linguistic annotation: either provided by human subject-matter experts, or
 * provided by machine annotators
 * @tparam L the type of linguistic entity the annotation is relevant to
 */
trait Annotation[L <: LinguisticEntity]

/**
 * Simple label-annotations for linguistic entities; can apply to
 * @param entity the liguistic entity being annotated
 * @param label the label annotating the entity
 * @tparam L the type of linguistic entity the annotation is relevant to
 * @tparam A the type used for labels
 */
case class Labeled[L <: LinguisticEntity, A](val entity: L, val label: A)
  extends Annotation[L]
