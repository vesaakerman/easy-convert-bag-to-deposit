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
package nl.knaw.dans.easy.bag2deposit

import java.nio.charset.Charset.defaultCharset
import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.bag2deposit.AbrRewriteRule.{ parseCsv, toXml }
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.csv.{ CSVFormat, CSVParser, CSVRecord }
import resource.managed

import scala.collection.JavaConverters._
import scala.xml.transform.RewriteRule
import scala.xml.{ Elem, MetaData, Node, Text }

case class AbrRewriteRule(cfgDir: File) extends RewriteRule {

  private val periodFile: File = cfgDir / "ABR-period.csv"
  private val periodMap = parseCsvFile(periodFile, "ddm:temporal")
  private val complexFile: File = cfgDir / "ABR-complex.csv"
  private val complexMap = parseCsvFile(complexFile, "ddm:subject")

  private def parseCsvFile(file: File, label: String): Map[String, Elem] = {
    parseCsv(file, toXml).toMap.map {
      case (key, xml) => (key, xml.copy(label = label))
    }
  }

  private def find(key: String, map: Map[String, Node], file: File): Node = {
    map.getOrElse(key, throw new Exception(s"$key not found in $file"))
  }

  private def isAbr(attr: MetaData) = {
    attr.prefixedKey == "xsi:type" && attr.value.mkString("").startsWith("abr:ABR")
  }

  override def transform(n: Node): Seq[Node] = n match {
    case Elem(_, "temporal", attr: MetaData, _, Text(key)) if isAbr(attr) => find(key, periodMap, periodFile)
    case Elem(_, "subject", attr: MetaData, _, Text(key)) if isAbr(attr) => find(key, complexMap, complexFile)
    case _ => n
  }
}
object AbrRewriteRule extends DebugEnhancedLogging {

  private def valueUri(uuid: String) = s"http://www.rnaproject.org/data/$uuid"

  private def toXml(record: CSVRecord): (String, Elem) = {
    val description = record.size() match {
      case 4 => record.get(3)
      case 5 => s"${ record.get(3) }, ${ record.get(4) }" // one description contains a separator
      case _ => throw new IllegalArgumentException(s"not expected number of columns: $record")
    }
    val xml = <label xml:lang="nl"
                           valueURI={ valueUri(record.get(2)) }
                           subjectScheme="Archeologisch Basis Register"
                           schemeURI={ "http://www.rnaproject.org" }
              >{ description }</label>

    (record.get(0), xml)
  }

  def parseCsv[T](file: File, extract: CSVRecord => T): Iterable[T] = {
    managed(CSVParser.parse(file.toJava, defaultCharset(), CSVFormat.RFC4180))
      .map(parse(_).map(extract))
      .tried
  }.unsafeGetOrThrow

  private def parse(parser: CSVParser): Iterable[CSVRecord] = {
    parser.asScala.filter(_.asScala.nonEmpty).drop(2)
  }
}

