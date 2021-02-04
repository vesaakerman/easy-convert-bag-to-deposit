/**
 * Copyright (C) 2020 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package nl.knaw.dans.easy.bag2deposit.ddm

import better.files.File
import nl.knaw.dans.easy.bag2deposit.ddm.AbrRewriteRule.parse
import nl.knaw.dans.easy.bag2deposit.parseCsv
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.csv.CSVRecord

import scala.util.matching.Regex
import scala.xml.transform.RewriteRule
import scala.xml.{ Elem, Node }

case class AbrRewriteRule(cfgFile: File, oldLabel: String, newLabel: String, scheme: String, schemeURI: String) extends RewriteRule with DebugEnhancedLogging {
  private val map = parse(cfgFile, newLabel, scheme, schemeURI)

  private def getAbrCode(value: String): String = {
    val matchBetweenBrackets: Regex = raw"\((.*)\)".r
    matchBetweenBrackets.findFirstMatchIn(value) match {
      case Some(m) => m.group(1)
      case None => value
    }
  }
  override def transform(node: Node): Seq[Node] = {
    if (!isAbr(node)) node
    else {
      val key = getAbrCode(node.text)
      map.getOrElse(key, <notImplemented>{ s"$key not found in ${ cfgFile.name }" }</notImplemented>)
    }
  }

  private def isAbr(node: Node) = {
    val attr = node.attributes
    node.label == oldLabel &&
      (
        (attr.prefixedKey == "xsi:type" && attr.value.mkString("").startsWith("abr:ABR")) ||
        (attr.get("valueURI").mkString("").startsWith("http://www.rnaproject.org/"))
      )
  }
}

object AbrRewriteRule {
  val nrOfHeaderLines = 2

  private def parse(file: File, label: String, scheme: String, schemeURI: String): Map[String, Elem] = {
    parseCsv(file, nrOfHeaderLines)
      .map(toXml(label, scheme, schemeURI))
      .toMap
  }

  private def valueUri(uuid: String) = s"https://data.cultureelerfgoed.nl/term/id/abr/$uuid"

  private def toXml(label: String, scheme: String, schemeURI: String)(record: CSVRecord): (String, Elem) = {
    val xml = <label xml:lang="nl"
                           valueURI={ valueUri(record.get(2)) }
                           subjectScheme={ scheme }
                           schemeURI={ schemeURI }
              >{ record.get(3) }</label>

    (record.get(0), xml.copy(label = label))
  }
}
