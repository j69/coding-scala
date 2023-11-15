package videos.code01_contrvariance

import org.scalatest.flatspec.AnyFlatSpec

class IsContrvarianceHardTest extends AnyFlatSpec {

  behavior of "IsContrvarianceHardTest"

  it should "process co-variance" in {
    IsContrvarianceHard.caseVariant()
  }

  it should "process in-variance" in {
    IsContrvarianceHard.caseInvariant()
  }

  it should "process contr-variance. case 1" in {
    IsContrvarianceHard.caseContrvariantCase1()
  }
}
