import java.math.BigInteger
import scala.collection.immutable.HashMap

abstract class Op;

case class ValueData(version: Long, tid: Long, value: Long) extends Op;
case class OpData(version: Long, tid: Long, value: List[Op]) extends Op;


class Parser(s: String) {

  var pos: Int = 0;

  def parseSinglePacket(): Op = {
    val version = BigInteger(next(3), 2).longValue()
    val tid = BigInteger(next(3), 2).longValue()
    val data = parseData(tid, version)
    return data
  }

  def next(n: Int): String = {
    val res = s.substring(pos, pos + n)
    pos = pos + n
    res;
  }

  def parseData(tid: Long, version: Long): Op = {
    if (tid == 4) {
      return valueData(version, tid)
    };
    operatorData(tid, version);
  }

  def operatorData(tid: Long, version: Long): Op = {
    if (next(1) == "1") {
      return listPacket(BigInteger(next(11), 2).longValue(), tid, version)
    }
    lenPacket(BigInteger(next(15), 2).longValue(), tid, version)
  }

  def lenPacket(n: Long, tid: Long, version: Long): Op = {
    val end = pos + n;
    var op = List[Op]();
    while (pos < end) {
      val packet = parseSinglePacket();
      op = op :+ packet;
    }
    new OpData(version, tid, op);
  }

  def listPacket(n: Long, tid: Long, version: Long): Op = {
    val op = (0 until n.toInt).map(_ => parseSinglePacket()).toList
    new OpData(version, tid, op);
  }

  def valueData(version: Long, tid: Long): Op = {
    var v = "";
    var group = next(5);
    while (group.substring(0, 1) == "1") {
      v = v + group.substring(1);
      group = next(5)
    }
    new ValueData(version, tid, BigInteger(v + group.substring(1), 2).longValue());
  }

  def sumVersion(packet: Op): Long = {
    return packet match {
      case ValueData(version, _, _) => version
      case OpData(version, _, list) => {
        version + list.map(l => sumVersion(l)).sum
      }
    }
  }

  def evaluate(packet: Op): Long = {
    packet match {
      case ValueData(_, _, data) => data
      case OpData(_, tid, data) => {
        val ll = data.map(l => evaluate(l))
        expr.get(tid.toInt).map(x => x.apply(ll)).get;
      }
    }
  }

  val expr = HashMap[Int,(List[Long]=>Long)](0 -> (ee=>ee.sum) ,
    1 -> (ee => ee.product),
    2 -> (ee => ee.min),
    3 -> (ee => ee.max),
    5 -> (ee => if (ee(0) > ee(1)) 1 else 0),
    6 -> (ee => if (ee(0) < ee(1)) 1 else 0),
    7 -> (ee => if (ee(0) == ee(1)) 1 else 0))
}

object App {

  def main(args: Array[String]): Unit = {
    testPart1();
    testPart2();
  }

  def testPart1(): Unit = {
    val s = "E0525D9802FA00B80021B13E2D4260004321DC648D729DD67B2412009966D76C0159ED274F6921402E9FD4AC1B0F652CD339D7B82240083C9A54E819802B369DC0082CF90CF9280081727DAF41E6A5C1B9B8E41A4F31A4EF67E2009834015986F9ABE41E7D6080213931CB004270DE5DD4C010E00D50401B8A708E3F80021F0BE0A43D9E460007E62ACEE7F9FB4491BC2260090A573A876B1BC4D679BA7A642401434937C911CD984910490CCFC27CC7EE686009CFC57EC0149CEFE4D135A0C200C0F401298BCF265377F79C279F540279ACCE5A820CB044B62299291C0198025401AA00021D1822BC5C100763A4698FB350E6184C00A9820200FAF00244998F67D59998F67D5A93ECB0D6E0164D709A47F5AEB6612D1B1AC788846008780252555097F51F263A1CA00C4D0946B92669EE47315060081206C96208B0B2610E7B389737F3E2006D66C1A1D4ABEC3E1003A3B0805D337C2F4FA5CD83CE7DA67A304E9BEEF32DCEF08A400020B1967FC2660084BC77BAC3F847B004E6CA26CA140095003900BAA3002140087003D40080022E8C00870039400E1002D400F10038C00D100218038F400B6100229500226699FEB9F9B098021A00800021507627C321006E24C5784B160C014A0054A64E64BB5459DE821803324093AEB3254600B4BF75C50D0046562F72B1793004667B6E78EFC0139FD534733409232D7742E402850803F1FA3143D00042226C4A8B800084C528FD1527E98D5EB45C6003FE7F7FCBA000A1E600FC5A8311F08010983F0BA0890021F1B61CC4620140EC010100762DC4C8720008641E89F0866259AF460C015D00564F71ED2935993A539C0F9AA6B0786008D80233514594F43CDD31F585005A25C3430047401194EA649E87E0CA801D320D2971C95CAA380393AF131F94F9E0499A775460";
    val res = hexToBin(s);
    val p = Parser(res);
    val s1 = p.parseSinglePacket();
    val version = p.sumVersion(s1)
    if(version == 940) {
      println("Part1 succes")
    } else {
      println("Part1 error")
    }

  }

  def testPart2(): Unit = {
    val s = "E0525D9802FA00B80021B13E2D4260004321DC648D729DD67B2412009966D76C0159ED274F6921402E9FD4AC1B0F652CD339D7B82240083C9A54E819802B369DC0082CF90CF9280081727DAF41E6A5C1B9B8E41A4F31A4EF67E2009834015986F9ABE41E7D6080213931CB004270DE5DD4C010E00D50401B8A708E3F80021F0BE0A43D9E460007E62ACEE7F9FB4491BC2260090A573A876B1BC4D679BA7A642401434937C911CD984910490CCFC27CC7EE686009CFC57EC0149CEFE4D135A0C200C0F401298BCF265377F79C279F540279ACCE5A820CB044B62299291C0198025401AA00021D1822BC5C100763A4698FB350E6184C00A9820200FAF00244998F67D59998F67D5A93ECB0D6E0164D709A47F5AEB6612D1B1AC788846008780252555097F51F263A1CA00C4D0946B92669EE47315060081206C96208B0B2610E7B389737F3E2006D66C1A1D4ABEC3E1003A3B0805D337C2F4FA5CD83CE7DA67A304E9BEEF32DCEF08A400020B1967FC2660084BC77BAC3F847B004E6CA26CA140095003900BAA3002140087003D40080022E8C00870039400E1002D400F10038C00D100218038F400B6100229500226699FEB9F9B098021A00800021507627C321006E24C5784B160C014A0054A64E64BB5459DE821803324093AEB3254600B4BF75C50D0046562F72B1793004667B6E78EFC0139FD534733409232D7742E402850803F1FA3143D00042226C4A8B800084C528FD1527E98D5EB45C6003FE7F7FCBA000A1E600FC5A8311F08010983F0BA0890021F1B61CC4620140EC010100762DC4C8720008641E89F0866259AF460C015D00564F71ED2935993A539C0F9AA6B0786008D80233514594F43CDD31F585005A25C3430047401194EA649E87E0CA801D320D2971C95CAA380393AF131F94F9E0499A775460";
    val res = hexToBin(s);
    val p = Parser(res);
    val s1 = p.parseSinglePacket();
    val sum = p.evaluate(s1)
    if(sum == 13476220616073L) {
      println("Part2 succes")
    } else {
      println("Part2 error")
    }
  }
  
  private def hexToBin(hex1: String): String = {
    var hex = hex1
    hex = hex.replaceAll("0", "0000")
    hex = hex.replaceAll("1", "0001")
    hex = hex.replaceAll("2", "0010")
    hex = hex.replaceAll("3", "0011")
    hex = hex.replaceAll("4", "0100")
    hex = hex.replaceAll("5", "0101")
    hex = hex.replaceAll("6", "0110")
    hex = hex.replaceAll("7", "0111")
    hex = hex.replaceAll("8", "1000")
    hex = hex.replaceAll("9", "1001")
    hex = hex.replaceAll("A", "1010")
    hex = hex.replaceAll("B", "1011")
    hex = hex.replaceAll("C", "1100")
    hex = hex.replaceAll("D", "1101")
    hex = hex.replaceAll("E", "1110")
    hex = hex.replaceAll("F", "1111")
    hex
  }

}