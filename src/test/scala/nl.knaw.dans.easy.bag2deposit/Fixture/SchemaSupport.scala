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
package nl.knaw.dans.easy.bag2deposit.Fixture

import better.files.StringExtensions
import org.scalatest.Assertions.fail

import java.net.UnknownHostException
import javax.xml.XMLConstants
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.SchemaFactory
import scala.util.{ Failure, Try }
import scala.xml.{ Node, PrettyPrinter, SAXParseException, Utility }

trait SchemaSupport {
  val schema: String

  // lazy vals for two reasons:
  // - schemaFile is set by concrete test class
  // - postpone loading until actually validating

  private lazy val triedSchema = Try(SchemaFactory
    .newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
    .newSchema(Array[Source](new StreamSource(schema)))
  )

  lazy val schemaIsAvailable: Boolean = triedSchema match {
    case Failure(e: SAXParseException) if e.getCause.isInstanceOf[UnknownHostException] =>
      println("UnknownHostException: " + e.getMessage)
      false
    case Failure(e: SAXParseException) if e.getMessage.contains("Cannot resolve") =>
      println("Probably an offline third party schema: " + e.getMessage)
      false
    case _ => true
  }

  def validate(xml: Node): Try[Unit] = {
    val serialized = Utility.serialize(xml).toString()
    triedSchema.getOrElse(fail("Please prefix the test with 'assume(schemaIsAvailable)' to ignore a failure due to not reachable schema's"))
    triedSchema.flatMap { schema =>
      val source = new StreamSource(serialized.inputStream)
      Try(schema.newValidator().validate(source))
    }
  }
}
