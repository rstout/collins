package collins

import collins.power._

import models._
import test._

import play.api.libs.json._
import play.api.libs.Files.TemporaryFile
import play.api.mvc._
import play.api.mvc.MultipartFormData._

import org.specs2._
import specification._
import matcher.Matcher

class PowerActionSpec extends ApplicationSpecification {

  "Power Action Specification".title

  args(sequential = true)

  "Apply a power action" should {

    "PowerOff" in {
      PowerAction.apply("PowerOff") must beEqualTo(PowerOff)
    }

    "PowerOn" in {
      PowerAction.apply("PowerOn") must beEqualTo(PowerOn)
    }

    "PowerSoft" in {
      PowerAction.apply("PowerSoft") must beEqualTo(PowerSoft)
    }

    "PowerState" in {
      PowerAction.apply("PowerState") must beEqualTo(PowerState)
    }

    "Identify" in {
      PowerAction.apply("Identify") must beEqualTo(Identify)
    }

    "Verify" in {
      PowerAction.apply("Verify") must beEqualTo(Verify)
    }

    "SysEventLog" in {
      PowerAction.apply("SysEventLog") must beEqualTo(SysEventLog)
    }

    "RebootSoft" in {
      PowerAction.apply("RebootSoft") must beEqualTo(RebootSoft)
    }

    "RebootHard" in {
      PowerAction.apply("RebootHard") must beEqualTo(RebootHard)
    }

    "FirthOfFifth should throw MatchError" in {
      PowerAction.apply("FirthOfFifth") must throwA[MatchError]
    }

  }

}
