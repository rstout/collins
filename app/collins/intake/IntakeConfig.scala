package collins.intake

import util.config.Configurable

object IntakeConfig extends Configurable {

  override val namespace = "intake"
  override val referenceConfigFilename = "intake_variables_reference.conf"

  def enabled = true

  def variables = {
    val s = getStringSet("params")
    logger.error("The intake variables loaded were " + s)
    s
  }

  override protected def validateConfig() {

      variables
  }

}
