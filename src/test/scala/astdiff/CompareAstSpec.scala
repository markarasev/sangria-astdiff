package astdiff

import org.parboiled2.ParserInput.StringBasedParserInput
import org.scalatest.{Matchers, WordSpec}
import sangria.ast._
import sangria.parser.DefaultSourceMapper

class CompareAstSpec extends WordSpec with Matchers {

  "Ast comparison" should {

    "tell two asts are equivalents" when {

      "there's no fields" in {
        val doc1 = Document(
          Vector.empty,
          Vector(Comment("a comment", Some(AstLocation(1, 1, 1)))),
          None,
          None
        )
        val doc2 = Document(
          Vector.empty,
          Vector.empty,
          Some(AstLocation(0, 0, 0)),
          Some(new DefaultSourceMapper("id", new StringBasedParserInput("")))
        )
        CompareAst.equivalence(doc1, doc2) shouldBe Equivalents
      }

      "there are input object type definitions" in {
        val doc1 = Document(
          Vector(
            InputObjectTypeDefinition(
              "human",
              Vector(InputValueDefinition("name", NamedType("string"), None))
            )),
          Vector.empty,
          None,
          None
        )
        val doc2 = Document(
          Vector(
            InputObjectTypeDefinition(
              "human",
              Vector(InputValueDefinition("name", NamedType("string"), None))
            )),
          Vector.empty,
          None,
          None
        )
        CompareAst.equivalence(doc1, doc2) shouldBe Equivalents
      }

    }

    "tell two asts are not equivalent and why" when {

      "definitions differ in size" in {
        val doc1 = Document(
          Vector(ObjectTypeDefinition("otd", Vector.empty, Vector.empty)),
          Vector(Comment("a comment", Some(AstLocation(1, 1, 1)))),
          None,
          None
        )
        val doc2 = Document(
          Vector.empty,
          Vector.empty,
          Some(AstLocation(0, 0, 0)),
          Some(new DefaultSourceMapper("id", new StringBasedParserInput("")))
        )
        CompareAst.equivalence(doc1, doc2) shouldEqual NotEquivalents(
          s"""
             |left:  Vector(type otd)
             |right: Vector()
        """.stripMargin
        )
      }

      "two definitions differ in content" in {
        val doc1 = Document(
          Vector(
            ObjectTypeDefinition(
              "droid",
              Vector.empty,
              Vector(
                FieldDefinition(
                  "primaryFunction",
                  NamedType("string"),
                  Vector.empty
                ))
            )),
          Vector.empty,
          None,
          None
        )
        val doc2 = Document(
          Vector(
            ObjectTypeDefinition(
              "human",
              Vector.empty,
              Vector(
                FieldDefinition("homePlanet", NamedType("string"), Vector.empty)
              )
            )),
          Vector.empty,
          None,
          None
        )
        CompareAst.equivalence(doc1, doc2) shouldEqual NotEquivalents(
          s"""
             |left:  Vector(type droid {primaryFunction:string})
             |right: Vector(type human {homePlanet:string})
        """.stripMargin
        )
      }

      "several definitions differ" in {
        val doc1 = Document(
          Vector(
            InterfaceTypeDefinition(
              "character",
              Vector(
                FieldDefinition("name", NamedType("string"), Vector.empty)
              )
            ),
            ObjectTypeDefinition(
              "droid",
              Vector.empty,
              Vector(
                FieldDefinition(
                  "primaryFunction",
                  NamedType("string"),
                  Vector.empty
                )
              )
            )
          ),
          Vector.empty,
          None,
          None
        )
        val doc2 = Document(
          Vector(
            InterfaceTypeDefinition(
              "character",
              Vector(
                FieldDefinition("name", NamedType("string"), Vector.empty)
              )
            ),
            ObjectTypeDefinition(
              "human",
              Vector.empty,
              Vector(
                FieldDefinition("homePlanet", NamedType("string"), Vector.empty)
              )
            ),
            EnumTypeDefinition("episode", Vector(EnumValueDefinition("JEDI")))
          ),
          Vector.empty,
          None,
          None
        )
        CompareAst.equivalence(doc1, doc2) shouldEqual NotEquivalents(
          s"""
             |left:  Vector(type droid {primaryFunction:string})
             |right: Vector(type human {homePlanet:string}, enum episode{JEDI})
        """.stripMargin
        )
      }

      "a field differ in two definitions" in {
        val doc1 = Document(
          Vector(
            InterfaceTypeDefinition(
              "character",
              Vector(
                FieldDefinition("name", NamedType("string"), Vector.empty)
              )
            )
          ),
          Vector.empty,
          None,
          None
        )
        val doc2 = Document(
          Vector(
            InterfaceTypeDefinition(
              "character",
              Vector(
                FieldDefinition("firstName", NamedType("string"), Vector.empty)
              )
            )
          ),
          Vector.empty,
          None,
          None
        )
        CompareAst.equivalence(doc1, doc2) shouldEqual NotEquivalents(
          s"""
             |left:  Vector(interface character{name:string})
             |right: Vector(interface character{firstName:string})
        """.stripMargin
        )
      }

    }

  }

}
