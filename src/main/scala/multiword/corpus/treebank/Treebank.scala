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

package multiword.corpus.treebank

import scala.collection.mutable

import multiword.corpus.core._

/** A sentence with treebank mark-up 
 * @tparam T the underlying unit used for tokens
 */
class TreebankSentence[T <: Immutable] (
    val tokens:Seq[Token[T]], val anno:Annotation[Sentence[T]])

/** A collection of annotated sentences
 * @tparam T the underlying unit used for tokens
 */
abstract class Treebank[T<: Immutable] extends Iterable[TreebankSentence[T]]

