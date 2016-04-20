package controllers

import java.util

import com.typesafe.config.{ConfigList, ConfigFactory}
import com.verisign.dnsa.api.schema._1.{ResourceRecordType, GetResourceRecordListType}
import play.api.Logger
import play.api.mvc._
import play.api.libs.json._
import services.WsdlBase
import com.verisign.dnsa.api.schema._2._

import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}

/**
 * Created by malam on 3/30/16.
 */
class WSDLController extends Controller with WsdlBase {

  def getFailOverInfoI(record: String, zone: Option[String] = None): GetFailOverInfoResType =  {
    val getFailOverInfo = new GetFailOverInfoType()

    getFailOverInfo.setOwner(record.substring(0, record.indexOf("agodatest.it")-1))
    getFailOverInfo.setZoneName("agodatest.it")
    zone match {
      case Some(s) => getFailOverInfo.setViewName(s)
      case _ => // don't do anything
    }
    getFailOverInfo.setResourceRecordType("A")

    getPort().getFailOverInfo(getFailOverInfo, getAuthInfo(), null, null)
  }

  def getFailOverInfo(record: String, zone: Option[String] = None) = Action {
    val response: GetFailOverInfoResType = getFailOverInfoI(record, zone)
    val respList = response.getFailOverInfo.getFailOverRRInfoList.getFailOverRRInfo

    respList.foreach(x => Logger.info(s"Target = ${x.getFailOverTarget}, Priority = ${x.getPriority}"))

    Ok(Json.obj("records" -> respList.map(x => x.getFailOverTarget)))
  }

  def getGeoLocationViewSets() = Action {
    val geoLocationViewSets: GetGeoLocationViewSetsType = new GetGeoLocationViewSetsType

    val response: GetGeoLocationViewSetsResType = getPort().getGeoLocationViewSets(geoLocationViewSets, getAuthInfo(), null, null)
    val respList = response.getGeoLocationViewSet

    respList.foreach(x => Logger.info(s"${x.getViewName}"))
    val zonesList = respList.map{x => x.getViewName}.flatten

    Ok(Json.obj("zones" -> zonesList))
  }

  def getFailOverInfoList() = Action { implicit request =>
    val getFailOverInfoList = new GetFailOverInfoListType()
    getFailOverInfoList.setViewName("EAST US")
    getFailOverInfoList.setZoneName("agodatest.it")

    val response: GetFailOverInfoListResType = getPort().getFailOverInfoList(getFailOverInfoList, getAuthInfo(), null, null)
    val respList = response.getFailOverInfo.map{ x =>
      val getResourceRecordList = new GetResourceRecordListType
      getResourceRecordList.setDomainName("agodatest.it")
      getResourceRecordList.setViewName("EAST US")
      getResourceRecordList.setResourceRecordType(ResourceRecordType.A)
      getResourceRecordList.setOwner(x.getOwner)

      val resourceResponse = getPort().getResourceRecordList(getResourceRecordList, getAuthInfo(), null, null)

      val dcMapping: Array[String] = getIpMapping.getOrElse(resourceResponse.getResourceRecord.head.getRData, "Not Found").split(":")

      Record(x.getOwner, dcMapping(0).toUpperCase, Try(dcMapping(1)) match {
        case Success(country) => country
        case Failure(ex) => ""
      })
    }.toList

    Ok(views.html.list(respList))
  }

  def update(from_ip: String, to_ip: String) = Action {
    val response: GetFailOverInfoResType = getFailOverInfoI(from_ip)

    val failOverInfoType: FailOverInfoType = new FailOverInfoType
    val list: FailOverInfoListType = new FailOverInfoListType

    response.getFailOverInfo.getFailOverRRInfoList.getFailOverRRInfo.map { x =>
      val failOverRRInfoType = new FailOverRRInfoType
      failOverRRInfoType.setFailOverTarget(x.getFailOverTarget)
      failOverRRInfoType.setPriority(x.getPriority)

      x.getFailOverTarget equals to_ip match {
        case true => failOverRRInfoType.setProbeStatusOverrideType(x.getProbeStatusOverrideType)
        case false => failOverRRInfoType.setProbeStatusOverrideType(ProbeStatusOverrideType.FORCE_FAIL)
      }

      list.getFailOverRRInfo.add(failOverRRInfoType)
    }

    failOverInfoType.setFailOverRRInfoList(list)
    failOverInfoType.setOwner(response.getFailOverInfo.getOwner)
    failOverInfoType.setZoneName(response.getFailOverInfo.getZoneName)

    failOverInfoType.setResourceRecordType("A")
    failOverInfoType.setProtocol(ProbeProtocolType.PING)

    val pingInfo: PingRequestInfoType = new PingRequestInfoType
    pingInfo.setNumberOfPacketsToSend(10)
    pingInfo.setPercentagePacketLossAllowed(80)
    failOverInfoType.setPingRequestInfo(pingInfo)

    Ok(getPort().updateFailOverInfo(failOverInfoType, getAuthInfo(), null, null).isCallSuccess.toString)
  }

  def getConfig() = Action {
    val mapping: Map[String, String] = getIpMapping

    println(mapping.getOrElse("203.160.137.21", "Not Found"))

    Ok("Just testing!!!")
  }

}

case class Record(record: String, pointsTo: String, country: String)
