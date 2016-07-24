package game.one.core

import com.badlogic.gdx.math._
import game.one.core.GameOne.Level

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


    def dist1(t: Float) = thickness
    def dist2(t: Float) = thickness


    val points = Range(0, segmentCount)
      .map(_.toFloat / segmentCount)
      .map({ t =>
        val v = new Vector2()
        val d = new Vector2()
        spline.valueAt(v, t)
        spline.derivativeAt(d, t)
        (v, d, t)
      })

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

    val (points1, points2) = filter(points)(_._1)
      .map({ case (v, d, t) =>
        d.nor().rotate90(1)
        val d1 = dist1(t)
        val d2 = dist2(t)
        (
          (new Vector2(v).mulAdd(d, d1), v, d1),
          (new Vector2(v).mulAdd(d, -d2), v, d2)
        )
      })
      .unzip

    def filter2(seq: Seq[(Vector2, Vector2, Float)]) = {
      (seq.last +: seq :+ seq.head)
        .sliding(3)
        .filter({
          case Seq((_, prev, _), (v, _, d), (_, next, _)) =>
            val d2 = d * d
            v.dst2(prev) >= d2 && v.dst2(next) >= d2
          case _ => ???
        })
        .map(_(1)._1)
        .toSeq
    }


    Platforms.loop(world, filter(filter2(points1))(identity).toArray)
    Platforms.loop(world, filter(filter2(points2))(identity).toArray)



  }

}
