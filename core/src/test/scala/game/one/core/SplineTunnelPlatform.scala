package game.one.core

import com.badlogic.gdx.math._
import com.github.davidmoten.rtree.{Entry, RTree}
import com.github.davidmoten.rtree.geometry.Line
import game.one.core.GameOne.Level
import scala.collection.JavaConversions._

/**
  * Created by martonpapp on 23/07/16.
  */
object SplineTunnelPlatform {

  val distanceThreshold = 0.006f
  val distanceThresholdSquare = distanceThreshold * distanceThreshold

  case class Params(
    controlPointCount: Int,
    segmentCount: Int,
    width: Float,
    height: Float,
    thickness: Float
  )

  def provider(params: Params) : Level = { (world, start) =>
    import params._

    val controlPoints = Stream.continually(
      new Vector2(
        start.origin.x + MathUtils.random(-width, width),
        start.origin.y + MathUtils.random(-height, height)
      )
    ).take(controlPointCount) :+
      new Vector2(
        start.origin.x,
        start.origin.y + start.height / 2
      )

    val spline = new CatmullRomSpline[Vector2](
      controlPoints.toArray,
      true
    )


    def dist(t: Float) = thickness
//    def dist2(t: Float) = thickness


    val points = Range(0, segmentCount)
      .map(_.toFloat / segmentCount)
      .map({ t =>
        val v = new Vector2()
        spline.valueAt(v, t)
        (v, t)
      })

    case class E1(
      mp1: Vector2,
      mp2: Vector2,
      d: Float,
      e1p1: Vector2,
      e1p2: Vector2,
      e2p1: Vector2,
      e2p2: Vector2
    )

    val e1s =
      (points :+ points.head)
        .sliding(2)
        .collect({
          case Seq((p1, t), (p2, _)) =>
            val n =
              new Vector2(p2)
                .sub(p1)
                .nor()
                .rotate90(1)

            val d = dist(t)

            E1(
              p1,
              p2,
              d,
              new Vector2(p1).mulAdd(n, d),
              new Vector2(p2).mulAdd(n, d),
              new Vector2(p1).mulAdd(n, -d),
              new Vector2(p2).mulAdd(n, -d)
            )
        })
        .toSeq

    case class Edge(
      p1: Vector2,
      p2: Vector2
    )
    val (points1, points2) =
      (e1s :+ e1s.head)
        .flatMap({ i =>
          import i._
          Seq(
            (e1p1, e2p1),
            (e1p2, e2p2)
          )
        })
        .unzip

    def nonSelfIntersectingSegments(ps: Seq[Vector2]) : Seq[Seq[Vector2]] = {

      type Data = Line

      val lines =
        (ps :+ ps.head)
          .sliding(2).map(_.toSeq)
          .collect({
            case Seq(p1, p2) =>
              Line.create(p1.x, p1.y, p2.x, p2.y)
          })

      val tree =
        RTree
          .create[Data, Line]()
          .add(
            asJavaIterable(
              lines.map({ line =>
                Entry.entry(line, line)
              }).toIterable
            )
          )

      def selfIntersects(line: Line) : Boolean = {
        tree.search(line).toBlocking.toIterable
          .filterNot({ entry =>
            entry.value() eq line
          })
          .nonEmpty
      }

      val (nonIntersect, headIntersect) = lines.span({ line =>
        !selfIntersects(line)
      })

      val reorg =
        (headIntersect ++ nonIntersect).toIterable.tail

      reorg.foldLeft(Seq(Seq(new Vector2(
        reorg.head.x1(),
        reorg.head.y1()
      ))))({ (segs, line) =>
        val p = new Vector2(line.x2(), line.y2())

        if (selfIntersects(line)) {
          Seq(p) +: segs
        } else {
          (p +: segs.head) +: segs.tail
        }

      })
    }

    def filter[T](items: Seq[T])(fn: T => Vector2) = {
      items match {
        case head +: tail =>
          tail.foldLeft(Seq(head))({ (seq, item) =>
            val h = fn(seq.head)
            val v = fn(item)

            if (h.dst2(v) < distanceThresholdSquare) {
              seq
            } else {
              item +: seq
            }
          }).reverse
        case _ => items
      }
    }


    def draw(lines: Seq[Seq[Vector2]]) = {
      lines match {
        case s @ Seq(single) =>
          Platforms.chain(world, filter(single)(identity).toArray)
        case _ =>
          lines
            .map(seg => filter(seg)(identity))
            .filter(_.size >= 2)
            .foreach({ seg =>
              Platforms.chain(world, seg.toArray)
            })
      }
    }

    def processPoints(points: Seq[Vector2]) = {
      draw(nonSelfIntersectingSegments(points))
    }

    processPoints(points1)
    processPoints(points2)




  }

}
