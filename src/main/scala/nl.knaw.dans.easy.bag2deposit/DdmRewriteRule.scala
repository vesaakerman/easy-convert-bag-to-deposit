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

import scala.xml.Node
import scala.xml.transform.RewriteRule

case class DdmRewriteRule(temporal: Map[String, String], complex: Map[String, String]) extends RewriteRule {

  override def transform(n: Node): Seq[Node] = n match {
    case <datasetState>{_}</datasetState> => <datasetState>{"deleted"}</datasetState>
    case <previousState>{_}</previousState> => <previousState>{"formerState"}</previousState>
    case <lastStateChange>{_}</lastStateChange> => <lastStateChange>{"changeDateString"}</lastStateChange>
    case _ => super.transform(n)
  }
}

