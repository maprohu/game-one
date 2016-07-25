package game.one.core

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.graphics.{GL20, OrthographicCamera}
import com.badlogic.gdx.math.Vector2
import com.badlogic.gdx.physics.box2d.BodyDef.BodyType
import com.badlogic.gdx.physics.box2d._
import com.badlogic.gdx.physics.box2d.joints.RevoluteJointDef
import com.badlogic.gdx.utils.Timer
import com.badlogic.gdx.utils.Timer.Task
import game.one.core.GameOne.PlayerProvider

/**
  * Created by martonpapp on 23/07/16.
  */
object SingleWheel {


  val wheelRadius = 0.1f
  val wheelOrigin = new Vector2(0, 0)

  val maxAngularVelocity = 100
  val wheelDensity = 1
  val angularImpulse = 0.1f * wheelDensity
  val wheelFriction = 20f
  val jumpImpulse = wheelDensity * 0.08f

  val provider : PlayerProvider = world => {
    val player = new SingleWheel(world)
    (
      player,
      Start(
        new Vector2(
          wheelOrigin.x,
          wheelOrigin.y - wheelRadius
        ),
        wheelRadius * 2,
        wheelRadius * 2
      ),
      player.wheel
    )
  }

}

class SingleWheel(world: World) extends Player {
  import SingleWheel._

  val wheel = {
    val bodyDef = new BodyDef()
    bodyDef.`type` = BodyType.DynamicBody
    bodyDef.position.set(wheelOrigin)

    val body = world.createBody(bodyDef)

    val circle = new CircleShape()
    circle.setRadius(wheelRadius);

    val fixtureDef = new FixtureDef()
    fixtureDef.shape = circle
    fixtureDef.density = wheelDensity
    fixtureDef.friction = wheelFriction
    fixtureDef.restitution = 0f

    val fixture = body.createFixture(fixtureDef)

    circle.dispose()

    body
  }


  val center = new Vector2(0, 0)
  val jump = new Vector2(0, jumpImpulse)
  val gravity = new Vector2(0, -10)

  override def step: Unit = {
    if (
      Gdx.input.isKeyPressed(Keys.LEFT)
        &&
        wheel.getAngularVelocity() < maxAngularVelocity
    ) {
      wheel.applyTorque(angularImpulse, true)
//      wheel.applyAngularImpulse(angularImpulse, true)
    }

    if (
      Gdx.input.isKeyPressed(Keys.RIGHT)
        &&
        wheel.getAngularVelocity() > -maxAngularVelocity
    ) {
      wheel.applyTorque(-angularImpulse, true)
//      wheel.applyAngularImpulse(-angularImpulse, true)
    }

    if (
      Gdx.input.isKeyJustPressed(Keys.UP)
    ) {
      wheel.applyLinearImpulse(
        jump,
        wheel.getWorldCenter,
        true
      )

//      gravity.scl(-1)
//      world.setGravity(gravity)
    }
  }

}
