package game.one.core

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{GL20, OrthographicCamera}
import com.badlogic.gdx.math.Vector2
import com.badlogic.gdx.physics.box2d.BodyDef.BodyType
import com.badlogic.gdx.physics.box2d._
import com.badlogic.gdx.physics.box2d.joints.RevoluteJointDef
import com.badlogic.gdx.utils.Timer
import com.badlogic.gdx.utils.Timer.Task

/**
  * Created by martonpapp on 23/07/16.
  */
object LargeSmallWheelParams {

  val frameRate = 1/60f

  val largeWheelRadius = 1
  val smallWheelRadius = 0.1f

  val axisDistance = largeWheelRadius * 2

  val largeWheelOrigin = new Vector2(0, 0)
  val smallWheelOrigin = new Vector2(largeWheelOrigin.x, largeWheelOrigin.y + axisDistance)

  val groundWidth = 50

  val screenWidth = 10

}
class LargeSmallWheel extends GameLogic {

  import LargeSmallWheelParams._

  val camera = new OrthographicCamera()

  val debugRenderer = new Box2DDebugRenderer()

  val world = {
    val world = new World(new Vector2(0, -10), true)

    val smallWheel = {
      val bodyDef = new BodyDef()
      bodyDef.`type` = BodyType.DynamicBody
      bodyDef.position.set(smallWheelOrigin)

      val body = world.createBody(bodyDef)

      val circle = new CircleShape()
      circle.setRadius(smallWheelRadius);

      val fixtureDef = new FixtureDef()
      fixtureDef.shape = circle
      fixtureDef.density = 0.5f
      fixtureDef.friction = 0.4f
      fixtureDef.restitution = 0.6f

      val fixture = body.createFixture(fixtureDef)

      circle.dispose()

      body
    }

    val largeWheel = {
      val bodyDef = new BodyDef()
      bodyDef.`type` = BodyType.DynamicBody
      bodyDef.position.set(largeWheelOrigin)

      val body = world.createBody(bodyDef)

      val circle = new CircleShape()
      circle.setRadius(largeWheelRadius);

      val fixtureDef = new FixtureDef()
      fixtureDef.shape = circle
      fixtureDef.density = 0.5f
      fixtureDef.friction = 0.4f
      fixtureDef.restitution = 0.6f

      val fixture = body.createFixture(fixtureDef)

      circle.dispose()

      body
    }

    val chassis = {
      val bodyDef = new BodyDef()
      bodyDef.`type` = BodyType.DynamicBody
      bodyDef.position.set(largeWheelOrigin)

      val body = world.createBody(bodyDef)

      val circle = new PolygonShape()
      circle.setAsBox(
        smallWheelRadius / 2,
        axisDistance / 2,
        new Vector2(0, axisDistance / 2 ),
        0
      )

      circle.setRadius(largeWheelRadius);

      val fixtureDef = new FixtureDef()
      fixtureDef.shape = circle
      fixtureDef.density = 0.5f
      fixtureDef.friction = 0.4f
      fixtureDef.restitution = 0.6f

      val fixture = body.createFixture(fixtureDef)

      circle.dispose()

      body
    }

    {
      val jointDef = new RevoluteJointDef
      jointDef.initialize(largeWheel, chassis, largeWheelOrigin)
      world.createJoint(jointDef)
    }

    {
      val jointDef = new RevoluteJointDef
      jointDef.initialize(smallWheel, chassis, smallWheelOrigin)
      world.createJoint(jointDef)
    }

    {
      val groundBodyDef = new BodyDef()

      val groundBody = world.createBody(groundBodyDef)

      val groundBox = new EdgeShape()
      groundBox.set(-groundWidth/2, -largeWheelRadius, groundWidth/2, -largeWheelRadius)
      groundBody.createFixture(groundBox, 0.0f)
      groundBox.dispose()
    }

    world
  }

  val timer = {
    val timer = new Timer
    timer.scheduleTask(
      new Task {
        override def run(): Unit = {
          world.step(frameRate, 6, 2)
        }
      },
      frameRate,
      frameRate
    )
    timer
  }

  def render = {
    Gdx.gl.glClearColor( 0, 0, 0, 1 )
    Gdx.gl.glClear( GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT )
    debugRenderer.render(world, camera.combined)
  }

  def resize(width: Int, height: Int): Unit = {
    val heightMeters = (height * screenWidth) / width
    camera.setToOrtho(false, screenWidth, heightMeters)
    camera.translate(-screenWidth / 2, -heightMeters / 2)
    camera.update()
  }

}
