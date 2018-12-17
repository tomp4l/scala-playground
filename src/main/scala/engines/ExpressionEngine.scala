package engines

import org.apache.commons.jexl3.{JexlBuilder, MapContext}

import scala.beans.BeanProperty


class Experian(
                @BeanProperty val creditScore: Int
                // ...
              )

case class Expression(name: String, definition: String)


sealed trait ExpressionResult
case class ExpressionBoolean(value: Boolean) extends ExpressionResult
case class ExpressionString(value: String) extends ExpressionResult
case class ExpressionDouble(value: Double) extends ExpressionResult
case class ExpressionInt(value: Int) extends ExpressionResult

object ExpressionEngine {

  private val engine = (new JexlBuilder)
    .cache(512)
    .strict(true)
    .silent(false)
    .create()

  def main(args: Array[String]): Unit = {
    val expressions = List(
      Expression("a", "experian.creditScore + 10"),
      Expression("b",
        """
          |if (calculated.a > 100) {
          | return true
          |} else {
          | return false
          |}
        """.stripMargin)
    )
    val dataProvider = Map("experian" -> new Experian(89))
    val outPuts = runExpressions(expressions, dataProvider)
    println(outPuts)
  }

  def runExpressions(expressions: List[Expression], dataProviders: Map[String, Any]): Map[String, ExpressionResult] = {
    val context = new MapContext()
    dataProviders.foreach { case (s, v) => context.set(s, v) }
    val calculatedVariables = new MapContext()
    context.set("calculated", calculatedVariables)
    val calculatedList = for (expression <- expressions) yield {
      val e = engine.createScript(expression.definition)
      val calculated = e.execute(context)
      calculatedVariables.set(expression.name, calculated)
      (expression.name, mapExpressionResult(calculated))
    }
    calculatedList.toMap
  }

  def mapExpressionResult(value: Any): ExpressionResult =
    value match {
      case b: Boolean => ExpressionBoolean(b)
      case s: String => ExpressionString(s)
      case d: Double => ExpressionDouble(d)
      case i: Int => ExpressionInt(i)
      case _ => throw new IllegalArgumentException()
    }
}
