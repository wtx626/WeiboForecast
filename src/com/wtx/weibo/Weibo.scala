package com.wtx.weibo

import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.io.Source

/**
  * Created by wutianxiong on 2017/4/8.
  */
object Weibo {

  def fit(lst: List[Array[String]]): (Int, Int, Int) = {
    var most = (0, 0, 0)
    lst.foreach(userList => {
      var denominator ,molecular = 0
      println(userList(2) + "\t" + userList(3) + "\t" + userList(4))
      val forward_count = Integer.parseInt(userList(2))
      val comment_count = Integer.parseInt(userList(3))
      val like_count = Integer.parseInt(userList(4))
      var max_precision = 0
      lst.foreach(tmp => {
        val forward_count_tmp = Integer.parseInt(tmp(2))
        val comment_count_tmp = Integer.parseInt(tmp(3))
        val like_count_tmp = Integer.parseInt(tmp(4))
        val df = Math.abs(forward_count - forward_count_tmp) / (forward_count_tmp + 5)
        val dc = Math.abs(comment_count - comment_count_tmp) / (comment_count_tmp + 3)
        val dl = Math.abs(like_count - like_count_tmp) / (like_count_tmp + 3)
        val precision_i = 1 - 0.5 * df - 0.25 * dc - 0.25 * dl
        var count_i = forward_count_tmp + comment_count_tmp + like_count_tmp
        if (count_i > 100) count_i = 100
        val sig_x = if (precision_i > 0.8) 1 else 0
        molecular += (count_i + 1) * sig_x
        denominator += count_i + 1
      })
      if (molecular / denominator > max_precision) {
        max_precision = molecular / denominator
        most = (forward_count, comment_count, like_count)
      }
    })
    most
  }

  def main(args: Array[String]): Unit = {
    //train
    val path = "/Users/wutianxiong/Downloads/SafariDownload/Weibo Data/weibo_train_data.txt"
    val lines = Source.fromFile(path).getLines.map(line => line.split("\t"))
    var sortByUser: Map[String, List[Array[String]]] = Map[String, List[Array[String]]]()
    lines.foreach(line => {
      if (sortByUser.contains(line(0))) {
        List.concat(sortByUser.get(line(0)), line.tail)
      } else {
        sortByUser += (line(0) -> List(line.tail))
      }
    })
    //answer is the result of training [uid,[forward_count,comment_count,like_count]]
    var answer: mutable.HashMap[String, (Int, Int, Int)] = mutable.HashMap[String, (Int, Int, Int)]()
    sortByUser.foreach(user => answer += (user._1 -> fit(user._2)))
    //predict
    val path_predict = "/Users/wutianxiong/Downloads/SafariDownload/Weibo Data/weibo_predict_data.txt"
    val path_result = "/Users/wutianxiong/Downloads/SafariDownload/Weibo Data/weibo_result_data.txt"
    val writer = new PrintWriter(new File(path_result))
    Source.fromFile(path_predict).getLines
      .map(line => line.split("\t"))
      .foreach(line => {
        val result = if (answer.get(line(0)) == null) (0, 0, 0) else answer.get(line(0)) match {
          case Some(truth) => truth
          case None => (0, 0, 0)
        }
        writer.println(line(0) + "\t" + line(1) + "\t" + result._1 + "," + result._2 + "," + result._3)
      })
    writer.close()
  }
}
