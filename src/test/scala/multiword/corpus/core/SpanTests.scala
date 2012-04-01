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

import org.scalatest.FunSuite

class SpanTestSuite extends FunSuite {

  test("spans equal") {
    assert(Span(0, 1) === Span(0, 1))
    assert(Span(1, 5) === Span(1, 5))
  }

  test("spans less than") {
    assert(Span(0, 1) < Span(0, 2))
    assert(Span(1, 2) < Span(0, 3))
    assert(Span(1, 2) < Span(0, 2))
  }

  test("spans less than or equals") {
    assert(Span(0, 1) <= Span(0, 2))
    assert(Span(1, 2) <= Span(0, 3))
    assert(Span(1, 2) <= Span(0, 2))
    assert(Span(0, 1) <= Span(0, 1))
    assert(Span(0, 3) <= Span(0, 3))
  }

  test("spans greater than") {
    assert(Span(0, 2) > Span(0, 1))
    assert(Span(0, 3) > Span(1, 2))
    assert(Span(0, 2) > Span(1, 2))
  }

  test("spans greater than or equals") {
    assert(Span(0, 2) >= Span(0, 1))
    assert(Span(0, 3) >= Span(1, 2))
    assert(Span(0, 2) >= Span(1, 2))
    assert(Span(0, 1) <= Span(0, 1))
    assert(Span(0, 3) <= Span(0, 3))
  }

  test("catching crossing bracket errors") {
    intercept[UncomparableSpans](Span(0, 2) < Span(1, 3))
    intercept[UncomparableSpans](Span(0, 2) <= Span(1, 3))
    intercept[UncomparableSpans](Span(0, 2) > Span(1, 3))
    intercept[UncomparableSpans](Span(0, 2) >= Span(1, 3))
  }
}

