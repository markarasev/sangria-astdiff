import org.scalactic.Equality
import org.scalactic.TripleEquals._
import sangria.ast._

package object astdiff {

  implicit object DocumentEquality extends Equality[Document] {
    override def areEqual(lhs: Document, b: Any): Boolean = b match {
      case rhs: Document =>
        haveSameElements(lhs.definitions, rhs.definitions)(DefinitionEquality)
      case _ => false
    }
  }

  implicit object DefinitionEquality extends Equality[Definition] {
    override def areEqual(lhs: Definition, b: Any): Boolean = (lhs, b) match {
      case (itd1: InterfaceTypeDefinition, itd2: InterfaceTypeDefinition) =>
        itd1.===(itd2)(InterfaceTypeDefinitionEquality)
      case (otd1: ObjectTypeDefinition, otd2: ObjectTypeDefinition) =>
        otd1.===(otd2)(ObjectTypeDefinitionEquality)
      case (etd1: EnumTypeDefinition, etd2: EnumTypeDefinition) =>
        etd1.===(etd2)(EnumTypeDefinitionEquality)
      case (iotd1: InputObjectTypeDefinition,
            iotd2: InputObjectTypeDefinition) =>
        iotd1.===(iotd2)(InputObjectTypeDefinitionEquality)
      case _ => false
    }
  }

  implicit object InterfaceTypeDefinitionEquality
      extends Equality[InterfaceTypeDefinition] {
    override def areEqual(lhs: InterfaceTypeDefinition, b: Any): Boolean =
      b match {
        case rhs: InterfaceTypeDefinition =>
          lhs.name == rhs.name &&
            haveSameElements(lhs.fields, rhs.fields)(FieldDefinitionEquality) &&
            haveSameElements(lhs.directives, rhs.directives)
        case _ => false
      }
  }

  implicit object ObjectTypeDefinitionEquality
      extends Equality[ObjectTypeDefinition] {
    override def areEqual(lhs: ObjectTypeDefinition, b: Any): Boolean =
      b match {
        case rhs: ObjectTypeDefinition =>
          lhs.name == rhs.name &&
            haveSameElements(lhs.interfaces, rhs.interfaces)(NamedTypeEquality) &&
            haveSameElements(lhs.fields, rhs.fields)(FieldDefinitionEquality) &&
            haveSameElements(lhs.directives, rhs.directives)(DirectiveEquality)
        case _ => false
      }
  }

  implicit object EnumTypeDefinitionEquality
      extends Equality[EnumTypeDefinition] {
    override def areEqual(lhs: EnumTypeDefinition, b: Any): Boolean = b match {
      case rhs: EnumTypeDefinition =>
        lhs.name == rhs.name &&
          haveSameElements(lhs.values, rhs.values)(EnumValueDefinitionEquality) &&
          haveSameElements(lhs.directives, rhs.directives)(DirectiveEquality)
      case _ => false
    }
  }

  implicit object InputObjectTypeDefinitionEquality
      extends Equality[InputObjectTypeDefinition] {
    override def areEqual(lhs: InputObjectTypeDefinition, b: Any): Boolean =
      b match {
        case rhs: InputObjectTypeDefinition =>
          lhs.name == rhs.name &&
            haveSameElements(lhs.fields, rhs.fields)(
              InputValueDefinitionEquality) &&
            haveSameElements(lhs.directives, rhs.directives)(DirectiveEquality)
        case _ => false
      }
  }

  implicit object NamedTypeEquality extends Equality[NamedType] {
    override def areEqual(lhs: NamedType, b: Any): Boolean = b match {
      case rhs: NamedType => lhs.name == rhs.name
      case _              => false
    }
  }

  implicit object FieldDefinitionEquality extends Equality[FieldDefinition] {
    override def areEqual(lhs: FieldDefinition, b: Any): Boolean = b match {
      case rhs: FieldDefinition =>
        lhs.name == rhs.name &&
          lhs.fieldType.===(rhs.fieldType)(TypeEquality) &&
          haveSameElements(lhs.arguments, rhs.arguments)(
            InputValueDefinitionEquality) &&
          haveSameElements(lhs.directives, rhs.directives)(DirectiveEquality)
      case _ => false
    }
  }

  implicit object DirectiveEquality extends Equality[Directive] {
    override def areEqual(lhs: Directive, b: Any): Boolean = b match {
      case rhs: Directive =>
        lhs.name == rhs.name &&
          haveSameElements(lhs.arguments, rhs.arguments)(ArgumentEquality)
      case _ => false
    }
  }

  implicit object EnumValueDefinitionEquality
      extends Equality[EnumValueDefinition] {
    override def areEqual(lhs: EnumValueDefinition, b: Any): Boolean = b match {
      case rhs: EnumValueDefinition =>
        lhs.name == rhs.name &&
          haveSameElements(lhs.directives, rhs.directives)(DirectiveEquality)
      case _ => false
    }
  }

  implicit object TypeEquality extends Equality[Type] {
    override def areEqual(lhs: Type, b: Any): Boolean = (lhs, b) match {
      case (nt1: NamedType, nt2: NamedType) =>
        nt1.===(nt2)(NamedTypeEquality)
      case (nnt1: NotNullType, nnt2: NotNullType) =>
        nnt1.===(nnt2)(NotNullTypeEquality)
      case (lt1: ListType, lt2: ListType) =>
        lt1.===(lt2)(ListTypeEquality)
      case _ => false
    }
  }

  implicit object InputValueDefinitionEquality
      extends Equality[InputValueDefinition] {
    override def areEqual(lhs: InputValueDefinition, b: Any): Boolean =
      b match {
        case rhs: InputValueDefinition =>
          lhs.name == rhs.name &&
            lhs.valueType.===(rhs.valueType)(TypeEquality) &&
            lhs.defaultValue.===(rhs.defaultValue)(optEquality(ValueEquality)) &&
            haveSameElements(lhs.directives, rhs.directives)(DirectiveEquality)
        case _ => false
      }
  }

  implicit object ArgumentEquality extends Equality[Argument] {
    override def areEqual(lhs: Argument, b: Any): Boolean = b match {
      case rhs: Argument =>
        lhs.name == rhs.name &&
          lhs.value.===(rhs.value)(ValueEquality)
      case _ => false
    }
  }

  implicit object ValueEquality extends Equality[Value] {
    override def areEqual(lhs: Value, b: Any): Boolean = (lhs, b) match {
      case (iv1: IntValue, iv2: IntValue)         => iv1.value == iv2.value
      case (biv1: BigIntValue, biv2: BigIntValue) => biv1.value == biv2.value
      case (fv1: FloatValue, fv2: FloatValue)     => fv1.value == fv2.value
      case (bdv1: BigDecimalValue, bdv2: BigDecimalValue) =>
        bdv1.value == bdv2.value
      case (sv1: StringValue, sv2: StringValue)     => sv1.value == sv2.value
      case (bv1: BooleanValue, bv2: BooleanValue)   => bv1.value == bv2.value
      case (ev1: EnumValue, ev2: EnumValue)         => ev1.value == ev2.value
      case (lv1: ListValue, lv2: ListValue)         => lv1.values == lv2.values
      case (vv1: VariableValue, vv2: VariableValue) => vv1.name == vv2.name
      case (_: NullValue, _: NullValue)             => true
      case (ov1: ObjectValue, ov2: ObjectValue) =>
        haveSameElements(ov1.fields, ov2.fields)(ObjectFieldEquality)
      case _ => false
    }
  }

  implicit object NotNullTypeEquality extends Equality[NotNullType] {
    override def areEqual(lhs: NotNullType, b: Any): Boolean = b match {
      case rhs: NotNullType => lhs.ofType.===(rhs.ofType)(TypeEquality)
      case _                => false
    }
  }

  implicit object ListTypeEquality extends Equality[ListType] {
    override def areEqual(lhs: ListType, b: Any): Boolean = b match {
      case rhs: ListType => lhs.ofType.===(rhs.ofType)(TypeEquality)
      case _             => false
    }
  }

  implicit object ObjectFieldEquality extends Equality[ObjectField] {
    override def areEqual(lhs: ObjectField, b: Any): Boolean = b match {
      case rhs: ObjectField => lhs.name == rhs.name
      case _                => false
    }
  }

  private def haveSameElements[T](left: Vector[T], right: Vector[T])(
      implicit equality: Equality[T]
  ): Boolean =
    leftDiffRight(left, right) == Vector.empty &&
      leftDiffRight(right, left) == Vector.empty

  def leftDiffRight[T](left: Vector[T], right: Vector[T])(
      implicit equality: Equality[T]
  ): Vector[T] = right.foldLeft(left) {
    case (diff, elt) => removeFirst(diff, elt)
  }

  private def removeFirst[T](from: Vector[T], toRemove: T)(
      implicit equality: Equality[T]
  ): Vector[T] = {
    var removed = false
    from.flatMap { elt =>
      if (!removed && toRemove === elt) {
        removed = true
        None
      } else Some(elt)
    }
  }

  private def optEquality[T](implicit equality: Equality[T]) =
    new Equality[Option[T]] {
      override def areEqual(lhs: Option[T], b: Any): Boolean = (lhs, b) match {
        case (Some(left), Some(right)) => left === right
        case (None, None)              => true
        case _                         => false
      }
    }

}
