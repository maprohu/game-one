package game.one.core

import com.badlogic.gdx.math.{MathUtils, Vector2}
import com.badlogic.gdx.physics.box2d.{BodyDef, ChainShape, World}

/**
  * Created by martonpapp on 23/07/16.
  */
object Platforms {

  def chain(world: World, points: Array[Vector2]) = {
    val groundBodyDef = new BodyDef()

    val groundBody = world.createBody(groundBodyDef)

    val shape = new ChainShape()


    shape.createChain(points)

    groundBody.createFixture(shape, 0.0f)
    shape.dispose()
  }

}
