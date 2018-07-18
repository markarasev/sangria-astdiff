package astdiff

import org.scalatest.{Matchers, WordSpec}
import sangria.ast._

class DiffSpec extends WordSpec with Matchers {

  "Diff" should {

    "not render descriptions" in {
      val left = Vector(
        ObjectTypeDefinition(
          "human",
          Vector.empty,
          Vector(FieldDefinition("name", NamedType("string"), Vector.empty)),
          description = Some(StringValue("a human being"))
        ))
      val right = Vector(
        ObjectTypeDefinition(
          "droid",
          Vector.empty,
          Vector(FieldDefinition("name", NamedType("string"), Vector.empty)),
          description = Some(StringValue("a droid machine"))
        ))
      val expected =
        """
          |left:  Vector(type human {name:string})
          |right: Vector(type droid {name:string})
        """.stripMargin
      NotEquivalents(left, right).diff shouldEqual expected
    }

  }

}
