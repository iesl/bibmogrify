package edu.umass.cs.iesl.bibmogrify.model

import com.weiglewilczek.slf4s.Logging
import org.scalatest.{FunSuite, BeforeAndAfter}

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

class PersonTest extends FunSuite with BeforeAndAfter with Logging {

/*
  test("Names invert without space") {
    assert(Person.cleanupNameNoPeriods("Smith,John").equals("John Smith"))
  }
  test("Names invert with space") {
    assert(Person.cleanupNameNoPeriods("Smith, John").equals("John Smith"))
  }
  test("Names don't invert with zero commas") {
    assert(Person.cleanupNameNoPeriods("John Smith").equals("John Smith"))
  }
  test("Names don't invert with two commas") {
    assert(Person.cleanupNameNoPeriods("Smith, John, PhD").equals("Smith, John, PhD"))
  }

  test("Names may match fully") {
    assert(Person.compatibleName("John Smith", "John Smith"))
  }

  test("Inverted names match") {
    assert(Person.compatibleName("Smith, John", "John Smith"))
  }

  test("Last names must match") {
    assert(!Person.compatibleName("John Smith", "John Jones"))
  }

  test("Suffixes not supported without comma") {
    assert(!Person.compatibleName("John Smith", "John Smith MD"))
  }

  test("Suffixes supported with comma") {
    assert(Person.compatibleName("John Smith", "John Smith, MD"))
  }

  test("First names must match") {
    assert(!Person.compatibleName("John Smith", "Jane Smith"))
  }

  test("First names may match as initial") {
    assert(Person.compatibleName("John Smith", "J Smith"))
  }

  test("First names may match as initial, inverted") {
    assert(Person.compatibleName("Smith,John", "J Smith"))
  }

  test("Adding middle name OK") {
    assert(Person.compatibleName("John Smith", "John Archibald Smith"))
  }

  test("Adding middle initial OK") {
    assert(Person.compatibleName("John Smith", "John A Smith"))
  }

  test("Adding middle name OK with first initial") {
    assert(Person.compatibleName("John Smith", "J Archibald Smith"))
  }

  test("Adding middle initial OK with first initial") {
    assert(Person.compatibleName("John Smith", "J A Smith"))
  }

  test("Wrong middle initials are incompatible") {
    assert(!Person.compatibleName("John A Smith", "John B Smith"))
  }

  test("Adding joined middle initial OK with first initial") {
    assert(Person.compatibleName("John Smith", "JA Smith"))
  }

  test("Initial vs joined middle initial are compatible") {
    assert(Person.compatibleName("J Smith", "JA Smith"))
  }

  test("Wrong joined middle initials are incompatible") {
    assert(!Person.compatibleName("JB Smith", "JA Smith"))
  }

  test("Wrong joined vs unjoined middle initials are indistinguishable from legitimate two-letter names") {
    assert(Person.compatibleName("E O Wilson", "Ed Wilson"))
  }

  test("Correct joined middle initial is compatible with full name") {
    assert(Person.compatibleName("Edward O. Wilson", "EO Wilson"))
  }

  test("Wrong joined middle initial is incompatible with full name") {
    assert(!Person.compatibleName("Edward O. Wilson", "EQ Wilson"))
  }
*/
}
