package services

import javax.xml.ws.{BindingProvider, Binding}

import com.typesafe.config.{Config, ConfigFactory}
import com.verisign.dnsa.api.wsdl._2.{DNSAV20_Service, DNSAV20}
import com.verisign.dnsa.auth.schema._1.{CustomerContextType, UserTokenType, AuthInfoType}
import javax.inject._

import scala.util.Try
import scala.collection.JavaConversions._

/**
 * Created by malam on 3/30/16.
 */
trait Base {
  def getUniqueId(): String
  def getPort(): DNSAV20
  def getAuthInfo(): AuthInfoType
}

@Singleton
trait WsdlBase extends Base {

  private val config: Config = ConfigFactory.load()

  private val wsdlUrl: String = config.getString("ws.wsdlURL")
  private val username: String = config.getString("ws.username")
  private val password: String = config.getString("ws.password")
  private val resellerCustomerId: String = Try(config.getString("ws.")).toOption.getOrElse("")
  private val ipMapping: Map[String, String] = getIpMappingConf toMap

  private def getIpMappingConf = {
    for (
      dc <- config.getStringList("ip.dc");
      product <- config.getStringList("ip.products")
    ) yield {
      val myVal = dc.split(":")
      println(s"ip.${myVal(0)}.${product}")
      config.getString(s"ip.${myVal(0)}.${product}") -> s"${dc}"
    }
  }

  def initAuth(): AuthInfoType = {
    val authInfo = new AuthInfoType
    authInfo.setUserToken(new UserTokenType)
    authInfo.getUserToken.setUserName(username)
    authInfo.getUserToken.setPassword(password)

    resellerCustomerId match {
      case "" =>
      case _ => {
        authInfo.setCustomerContext(new CustomerContextType())
        authInfo.getCustomerContext.setResellerCustomerId(resellerCustomerId)
      }
    }

    authInfo
  }

  def initPort(): DNSAV20 = {
    val dnsV20_Service: DNSAV20_Service = new DNSAV20_Service()
    val port: DNSAV20 = dnsV20_Service.getDNSAV20

    val bindingProvider: BindingProvider = port.asInstanceOf[BindingProvider]
    val binding: Binding = bindingProvider.getBinding
    val handlerList = binding.getHandlerChain
    handlerList.add(new SOAPLoggingHandler)
    binding.setHandlerChain(handlerList)

    port
  }

  private val port: DNSAV20 = initPort()
  private val authInfo: AuthInfoType = initAuth()


  override def getUniqueId(): String = "" + System.currentTimeMillis()

  override def getPort(): DNSAV20 = port

  override def getAuthInfo(): AuthInfoType = authInfo

  def getIpMapping = ipMapping
}
