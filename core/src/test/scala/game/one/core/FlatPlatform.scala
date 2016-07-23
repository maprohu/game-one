package game.one.core
import com.badlogic.gdx.physics.box2d.{BodyDef, EdgeShape, World}
import game.one.core.GameOne.Level

/**
  * Created by martonpapp on 23/07/16.
  */
object FlatPlatform  {
  def provider(groundWidth: Float) : Level = (world, start) => {
    {
      val groundBodyDef = new BodyDef()

      val groundBody = world.createBody(groundBodyDef)

      val shape = new EdgeShape()
      shape.set(-groundWidth/2, start.origin.y, groundWidth/2, start.origin.y)
      groundBody.createFixture(shape, 0.0f)
      shape.dispose()
    }
  }
}
