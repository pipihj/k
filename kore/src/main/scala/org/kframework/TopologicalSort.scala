// Copyright (c) Runtime Verification, Inc. All Rights Reserved.
package org.kframework

import scala.annotation.tailrec
import scala.collection.immutable

/**
 * Created by dwightguth on 4/16/15.
 */
object TopologicalSort {
  def tsort[A](edges: Iterable[(A, A)]): Iterable[A] = {
    @tailrec
    def tsort(toPreds: Map[A, Set[A]], done: Iterable[A]): Iterable[A] = {
      val (noPreds, hasPreds) = toPreds.partition(_._2.isEmpty)
      if (noPreds.isEmpty) {
        if (hasPreds.isEmpty) done else sys.error(hasPreds.toString)
      } else {
        val found = noPreds.keys
        tsort(hasPreds.view.mapValues(_ -- found).toMap, done ++ found)
      }
    }

    val toPred = edges.foldLeft(Map[A, Set[A]]()) { (acc, e) =>
      acc + (e._1 -> acc.getOrElse(e._1, Set())) + (e._2 -> (acc.getOrElse(e._2, Set()) + e._1))
    }
    tsort(toPred, immutable.Seq())
  }
}
