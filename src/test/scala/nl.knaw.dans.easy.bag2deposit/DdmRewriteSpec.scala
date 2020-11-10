package nl.knaw.dans.easy.bag2deposit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.xml.{ Node, Utility }

class DdmRewriteSpec extends AnyFlatSpec with Matchers {
  "transform" should "convert" in {
    val ddmIn = ddm(
        <ddm:dcmiMetadata>
            <dcterms:temporal xsi:type="abr:ABRperiode">VMEA</dcterms:temporal>
            <dc:subject xsi:type="abr:ABRcomplex">EGVW</dc:subject>
            <dcterms:subject xsi:type="abr:ABRcomplex">ELA</dcterms:subject>
        </ddm:dcmiMetadata>
    )
    DdmRewriteRule(Map.empty, Map.empty)
      .transform(ddmIn).headOption.map(normalized)
      .getOrElse(fail("no DDM returned")) shouldBe normalized(ddm(
        <ddm:dcmiMetadata>
            <ddm:temporal xml:lang="en"
                          valueURI="http://www.rnaproject.org/data/330e7fe0-a1f7-43de-b448-d477898f6648"
                          subjectScheme="Archeologisch Basis Register"
                          schemeURI="http://www.rnaproject.org"
            >VMEA</ddm:temporal>
            <ddm:subject xml:lang="nl"
                         valueURI="http://www.rnaproject.org/data/6ae3ab19-49ca-44a7-8b65-3a3395014bb9"
                         subjectScheme="Archeologisch Basis Register"
                         schemeURI="http://www.rnaproject.org"
            >GW.VW</ddm:subject>
            <ddm:subject xml:lang="nl"
                         valueURI="http://www.rnaproject.org/data/f182d72c-2d22-47ae-b799-26dea01e770c"
                         subjectScheme="Archeologisch Basis Register"
                         schemeURI="http://www.rnaproject.org"
            >APVV.LA</ddm:subject>
        </ddm:dcmiMetadata>
    ))
  }

  val nameSpaceRegExp = """ xmlns:[a-z-]+="[^"]*"""" // these attributes have a variable order

  private def normalized(elem: Node) = printer
    .format(Utility.trim(elem)) // this trim normalizes <a/> and <a></a>
    .replaceAll(nameSpaceRegExp, "") // the random order would cause differences in actual and expected
    .replaceAll(" +\n?", " ")
    .replaceAll("\n +<", "\n<")
    .trim

  private def ddm (dcmi: Node) =
      <ddm:DDM xmlns:dcx="http://easy.dans.knaw.nl/schemas/dcx/"
         xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xmlns:abr="http://www.den.nl/standaard/166/Archeologisch-Basisregister/"
         xmlns:dc="http://purl.org/dc/elements/1.1/"
         xmlns:dct="http://purl.org/dc/terms/"
         xmlns:dcterms="http://purl.org/dc/terms/"
         xmlns:dcmitype="http://purl.org/dc/dcmitype/"
         xsi:schemaLocation="
         http://easy.dans.knaw.nl/schemas/md/ddm/ http://easy.dans.knaw.nl/schemas/md/2015/12/ddm.xsd
         http://www.den.nl/standaard/166/Archeologisch-Basisregister/ http://easy.dans.knaw.nl/schemas/vocab/2012/10/abr-type.xsd
         http://www.w3.org/2001/XMLSchema-instance http://easy.dans.knaw.nl/schemas/md/emd/2013/11/xml.xsd
         http://purl.org/dc/terms/ http://easy.dans.knaw.nl/schemas/emd/2013/11/qdc.xsd
         http://purl.org/dc/elements/1.1/ http://dublincore.org/schemas/xmls/qdc/dc.xsd
      "> { dcmi }
      </ddm:DDM>
}
