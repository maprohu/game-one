package game.one.core


import com.badlogic.gdx.{ApplicationAdapter, Gdx}
import com.badlogic.gdx.backends.lwjgl.LwjglApplication
import com.badlogic.gdx.graphics.{GL20, OrthographicCamera}
import com.badlogic.gdx.math.{Matrix4, Vector2}
import com.badlogic.gdx.physics.box2d.BodyDef.BodyType
import com.badlogic.gdx.physics.box2d._
import com.badlogic.gdx.utils.Timer
import com.badlogic.gdx.utils.Timer.Task

/**
  * Created by martonpapp on 23/07/16.
  */
object RunGameOne {


  def main(args: Array[String]) {

    Box2D.init()



    val listener = new ApplicationAdapter {
      var logic : Logic = null
      override def create(): Unit = {
        logic = new Logic
      }

      override def render(): Unit = {
        logic.render
      }

      override def resize(width: Int, height: Int): Unit = {
        logic.resize(width, height)
      }
    }
    val app = new LwjglApplication(listener)
  }

}

object Params {

  val frameRate = 1/60f

  val screenWidth = 10

  val largeWheelRadius = screenWidth / 10

  val axis

  val wheelOrigin = new Vector2(0, 0)

  val groundWidth = 50

}

class Logic {
  import Params._

  val camera = new OrthographicCamera()

  val debugRenderer = new Box2DDebugRenderer()

  val world = {
    val world = new World(new Vector2(0, -10), true)

    {
      val bodyDef = new BodyDef()
      bodyDef.`type` = BodyType.DynamicBody
      bodyDef.position.set(wheelOrigin)

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
    }

    {
      val bodyDef = new BodyDef()
      bodyDef.`type` = BodyType.DynamicBody
      bodyDef.position.set(wheelOrigin)

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
          println("step")
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
    println("render")
  }

  def resize(width: Int, height: Int): Unit = {
    val heightMeters = (height * screenWidth) / width
    camera.setToOrtho(false, screenWidth, heightMeters)
    camera.translate(-screenWidth / 2, -heightMeters / 2)
    camera.update()
  }
}
