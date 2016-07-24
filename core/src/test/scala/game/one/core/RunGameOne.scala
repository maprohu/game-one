package game.one.core


import com.badlogic.gdx.{ApplicationAdapter, Gdx}
import com.badlogic.gdx.backends.lwjgl.LwjglApplication
import com.badlogic.gdx.graphics.{GL20, OrthographicCamera}
import com.badlogic.gdx.math.{Matrix4, Vector2, Vector3}
import com.badlogic.gdx.physics.box2d._
import com.badlogic.gdx.utils.Timer
import com.badlogic.gdx.utils.Timer.Task
import game.one.core.RandomLinePlatform.Params

/**
  * Created by martonpapp on 23/07/16.
  */
object GameOne {
  type PlayerProvider = World => (Player, Start, Body)
  type Level = (World, Start) => Unit
  type LogicProvider = () => GameLogic

}
import GameOne._

object RunGameOne {


  val logicProvider = {
    //    val level = FlatPlatform.provider(50)
    val playerProvider = SingleWheel.provider

//    val level = RandomLinePlatform.provider(
//      Params(
//        100,
//        SingleWheel.wheelRadius * 2,
//        SingleWheel.wheelRadius * 5,
//        SingleWheel.wheelRadius * 2
//      )
//    )

//    val level = SplineTunnelPlatform.test

    val level = SplineTunnelPlatform.provider(
      SplineTunnelPlatform.Params(
        4,
        50,
        SingleWheel.wheelRadius * 100,
        SingleWheel.wheelRadius * 30,
        SingleWheel.wheelRadius * 2
      )
    )

    val screenWidth = 10

    { () =>
      new PlayerLevelLogic(
        playerProvider,
        level,
        screenWidth
      )
    }
  }


  def main(args: Array[String]) {

    Box2D.init()



    val listener = new ApplicationAdapter {
      var logic : GameLogic = null
      override def create(): Unit = {
        logic = logicProvider()
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


trait Player {

  def step : Unit

}


trait GameLogic {
  def render : Unit
  def resize(width: Int, height: Int): Unit
}

case class Start(
  origin: Vector2,
  width: Float,
  height: Float
)



class PlayerLevelLogic(
  playerProvider: PlayerProvider,
  level: Level,
  screenWidth: Float,
  frameRate : Float = 1/60f
) extends GameLogic {

  val world = new World(new Vector2(0, -10), true)

  val (player, start, playerBody) = playerProvider(world)
  val playerPosition3 = new Vector3()

  level(world, start)

  val timer = {
    val timer = new Timer
    timer.scheduleTask(
      new Task {
        override def run(): Unit = {
          player.step
          world.step(frameRate, 6, 2)
          world.clearForces()
          playerPosition3.set(playerBody.getPosition, 0)
          camera.position.lerp(playerPosition3, 0.1f)
          camera.update(false)
        }
      },
      frameRate,
      frameRate
    )
    timer
  }

  val camera = new OrthographicCamera()

  val debugRenderer = new Box2DDebugRenderer()

  def render = {
    Gdx.gl.glClearColor( 0, 0, 0, 1 )
    Gdx.gl.glClear( GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT )
    debugRenderer.render(world, camera.combined)
  }

  def resize(width: Int, height: Int): Unit = {
    val heightMeters = (height * screenWidth) / width
    camera.setToOrtho(false, screenWidth, heightMeters)
    camera.position.set(playerBody.getPosition, 0)
    camera.update()
  }

}

