package game.one.core

import com.badlogic.gdx.math.{MathUtils, Vector2}
import com.badlogic.gdx.physics.box2d.{BodyDef, ChainShape, EdgeShape}
import game.one.core.GameOne.Level

/**
  * Created by martonpapp on 23/07/16.
  */
object RandomLinePlatform  {

  case class Params(
    count: Int,
    minWidth: Float,
    maxWidth: Float,
    maxElevation: Float
  )

  def provider(params: Params) : Level = (world, start) => {
    import params._
    {
      val groundBodyDef = new BodyDef()

      val groundBody = world.createBody(groundBodyDef)

      val shape = new ChainShape()

      val firstWidth = MathUtils.random(
        Math.max(
          minWidth,
          start.width
        ),
        Math.max(
          maxWidth,
          start.width
        )
      )

      def next(start: Vector2, min: Float, max: Float) : Vector2 = {
        new Vector2(
          start.x + MathUtils.random(min, max),
          start.y + MathUtils.random(-maxElevation, maxElevation)
        )
      }

      def half(start: Vector2, min: Float, max: Float) : Seq[Vector2] = {
        Stream.iterate((Seq(start)))({ seq =>
          next(seq.head, min, max) +: seq
        })(count)
      }

      shape.createChain(
        (
          half(
            new Vector2(
              start.origin.x - firstWidth / 2,
              start.origin.y
            ),
            -minWidth,
            -maxWidth
          ) ++ half(
            new Vector2(
              start.origin.x + firstWidth / 2,
              start.origin.y
            ),
            minWidth,
            maxWidth
          ).reverse
        ).toArray

      )


      groundBody.createFixture(shape, 0.0f)
      shape.dispose()
    }
  }
}

