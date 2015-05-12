package collins.graphs

import models.asset.AssetView
import util.config.TypesafeConfiguration

import play.api.{Application, Plugin}
import play.api.mvc.Content

class GraphPlugin(override val app: Application) extends GraphView with Plugin {

  protected var underlying: Option[GraphView] = None

  override def enabled = {
    GraphConfig.pluginInitialize(app.configuration)
    GraphConfig.enabled
  }
  override def onStart() {
    val instance = getGraphInstance()
    instance.validateConfig()
    underlying = Some(instance)
  }
  override def onStop() {
    underlying = None
  }

  override def isGraphable(asset: AssetView): Boolean = {
    if(enabled){
      return underlying.get.isGraphable(asset)
    }
    return false
  }

  override def get(asset: AssetView): Option[Content] = {
    underlying.flatMap(_.get(asset))
  }

  protected def getGraphInstance(): GraphView = {
    this.getClass.getClassLoader.loadClass(GraphConfig.className)
      .getConstructor(classOf[Application])
      .newInstance(app)
      .asInstanceOf[GraphView]
  }

}
