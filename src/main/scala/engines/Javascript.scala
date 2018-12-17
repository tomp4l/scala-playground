package engines

import java.util

import javax.script.{Compilable, Invocable, ScriptEngineManager}


case class DataProvider(name: String)

case class JVar(identifier: String, expression: String)

case class Rule(name: String, dataProviders: List[DataProvider], definition: String)

object Javascript {
  def main(args: Array[String]): Unit = {

    val expressions = List(
      JVar("a", "1 + 2"),
      JVar("b", "a * 3")
    )

    val rules = List(
      Rule("testA", List(), "return a"),
      Rule("testB", List(DataProvider("experian")), "return experian.a + b"),
      Rule("testC", List(DataProvider("experian")),
        """
          |if (experian.creditScore > 100) {
          |   return "ACCEPT"
          |} else {
          |   return "DECLINE"
          |}
        """.stripMargin)
    )

    val experianData = new util.HashMap[String, Any]()
    experianData.put("a", 2)
    experianData.put("creditScore", 123)

    val engine = new ScriptEngineManager().getEngineByName("nashorn")
    val script = makeScript(expressions, rules)
    engine.eval(script)

    val ruleOutcomes = runRules(engine.asInstanceOf[Invocable], rules, Map("experian" -> experianData))

    println(script)
    println()
    ruleOutcomes.foreach(println)
  }

  def runRules(invocable: Invocable, rules: List[Rule], dataMap: Map[String, util.HashMap[String, Any]]): List[AnyRef] = rules.map(
    r =>invocable.invokeFunction(r.name, r.dataProviders.map(d => dataMap(d.name)):_*)
  )

  def makeScript(expressions: List[JVar], rules: List[Rule]): String = {
    makeVals(expressions) ++ "\n" ++ makeFunctions(rules)
  }

  def makeVals(expressions: List[JVar]) : String =
    expressions.
      map({ case JVar(name, value) => s"var $name = $value;" })
      .mkString("\n")

  def makeFunctions(rules: List[Rule]): String =
    rules.
      map({
        case Rule(name, dataProviders, definition) => {
          val parameterList = dataProviders.map(_.name).mkString(" ,")
          s"function $name($parameterList) {\n$definition\n}"
        }
      }).mkString("\n")
}
