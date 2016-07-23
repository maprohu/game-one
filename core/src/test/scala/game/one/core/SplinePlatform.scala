package game.one.core

import com.badlogic.gdx.math._
import game.one.core.GameOne.Level

/**
  * Created by martonpapp on 23/07/16.
  */
object SplinePlatform {

  case class Params(
    controlPointCount: Int,
    segmentCount: Int,
    width: Float,
    height: Float
  )

  def provider(params: Params) : Level = { (world, start) =>
    import params._

    val controlPoints = Stream.continually(
      new Vector2(
        start.origin.x + MathUtils.random(-width, width),
        start.origin.y + MathUtils.random(-height, height)
      )
    ).take(controlPointCount)

    val spline = new CatmullRomSpline[Vector2](
      controlPoints.toArray,
      true
    )

    val points = Range(0, segmentCount)
      .map(_.toFloat / segmentCount)
      .map({ t =>
        val v = new Vector2()
        spline.valueAt(v, t)
        v
      })

    Platforms.chain(world, points.toArray)



  }

}
