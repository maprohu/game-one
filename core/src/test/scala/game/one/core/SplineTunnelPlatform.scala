package game.one.core

import java.awt.geom.Line2D

import com.badlogic.gdx.math._
import com.badlogic.gdx.physics.box2d.World
import com.github.davidmoten.rtree.{Entry, RTree}
import com.github.davidmoten.rtree.geometry.{Geometry, Line, Point, Rectangle}
import com.vividsolutions.jts.geom.{Coordinate, GeometryFactory, LineString}
import game.one.core.GameOne.Level
import game.one.core.SplineTunnelPlatform.Params
import rx.functions.Func2

import scala.collection.JavaConversions._
import scala.collection.immutable._

/**
  * Created by martonpapp on 23/07/16.
  */
object SplineTunnelPlatform {

  val distanceThreshold = 0.006f
  val distanceThresholdSquare = distanceThreshold * distanceThreshold

  def isTooClose(v1: Vector2, v2: Vector2) : Boolean = {
    v1.dst2(v2) < distanceThresholdSquare
  }


  case class Params(
    controlPointCount: Int,
    segmentCount: Int,
    width: Float,
    height: Float,
    thickness: Float
  )

  def provider(params: Params) : Level = { (world, start) =>
    new SplineLevel(params, world, start)
  }

  def test : Level = { (world, start) =>
    performCut(
      Seq(
        new Vector2(-1, 0),
        new Vector2(1, 0),
        new Vector2(0, 1)
      ).map(v => v.add(start.origin)),
      0.2f,
      edgeDrawer(world)
    )
  }


  // TODO: different cutter for each side
  class Cutter(
    val polygon: Polygon
  ) extends Geometry {

//    val rectangle = Rectangle.create(
//      Math.min(
//        line.x1(),
//        line.x2()
//      ) - d,
//      Math.min(
//        line.y1(),
//        line.y2()
//      ) - d,
//      Math.max(
//        line.x1(),
//        line.x2()
//      ) + d,
//      Math.max(
//        line.y1(),
//        line.y2()
//      ) + d
//    )

//    def isInside(x: Float, y: Float) : Boolean = {
//      Line2D.ptSegDistSq(
//        line.x1(),
//        line.y1(),
//        line.x2(),
//        line.y2(),
//        x,
//        y
//      ) < d*d
//    }

    def isInside(x: Float, y: Float) : Boolean = {
      polygon.contains(x, y)
    }

    override def distance(r: Rectangle): Double = ???

    val bb = polygon.getBoundingRectangle

    override def intersects(r: Rectangle): Boolean = {
      val rr = new com.badlogic.gdx.math.Rectangle(
        r.x1(),
        r.y1(),
        r.x2() - r.x1(),
        r.y2() - r.y1()
      )
      bb.overlaps(
        rr
      )
//      val rp = new Polygon(
//        Array(
//          r.x1(), r.y1(),
//          r.x2(), r.y1(),
//          r.x2(), r.y2(),
//          r.x1(), r.y2()
//        )
//      )
//      Intersector.overlapConvexPolygons(
//        polygon,
//        rp
//      )
//      (line.distance(r) < d).asInstanceOf[Boolean]
    }

    val br = {
      val br = polygon.getBoundingRectangle
      Rectangle.create(
        br.x,
        br.y,
        br.x + br.width,
        br.y + br.height
      )
    }

    override def mbr(): Rectangle = {
      br
    }
  }

  class Vertex(
    val v: Vector2,
    val cutters: Set[Cutter] = Set()
  ) {
    def point = Point.create(v.x, v.y)
  }
  object Vertex {
    def apply(p: Vector2): Vertex = new Vertex(p, Set())
    def apply(p: Vector2, cutter: Cutter): Vertex = new Vertex(p, Set(cutter))
  }

  case class Edge(
    p1: Vertex,
    p2: Vertex
  ) {
    def line = Line.create(p1.v.x, p1.v.y, p2.v.x, p2.v.y)
    val cutters = p1.cutters intersect p2.cutters
  }



  def performCut(
    points: Seq[Vector2],
    dist: Float,
    drawEdges: Seq[Edge] => Unit
  ) = {
    val gf = new GeometryFactory()

    val ls = gf.createLineString(
      (points :+ points.head)
        .map({ p =>
          new Coordinate(p.x, p.y)
        })
        .toArray
    )

    val buffered = ls.buffer(dist)


    def drawLineString(ls: LineString) = {
      drawEdges(
        ls
          .getCoordinates
          .sliding(2)
          .map({
            case Array(c1, c2) =>
              Edge(
                Vertex(new Vector2(
                  c1.x.toFloat,
                  c1.y.toFloat
                )),
                Vertex(new Vector2(
                  c2.x.toFloat,
                  c2.y.toFloat
                ))
              )
          })
          .to[Seq]
      )
    }

    buffered match {
      case p : com.vividsolutions.jts.geom.Polygon =>
        drawLineString(p.getExteriorRing)
        Range(0, p.getNumInteriorRing).foreach({ idx =>
          drawLineString(p.getInteriorRingN(idx))
        })
    }
  }

  def edgeDrawer(world: World) = {

    def filter[T](items: Seq[T])(fn: T => Vector2) = {
      items match {
        case head +: tail =>
          tail.foldLeft(Seq(head))({ (seq, item) =>
            val h = fn(seq.head)
            val v = fn(item)

            if (isTooClose(h, v)) {
              seq
            } else {
              item +: seq
            }
          }).reverse
        case _ => items
      }
    }

    def drawChainOrLoop(points: Seq[Vector2]) : Unit = {
      val filtered = filter(points)(identity)
      if (filtered.size <= 1) return
      if (isTooClose(filtered.head, filtered.last)) {
        Platforms.loop(world, filtered.tail.toArray)
      } else {
        Platforms.chain(world, filtered.toArray)
      }
    }

    def drawVertices(vertices: Seq[Vertex]) = {
      drawChainOrLoop(vertices.map(_.v))
    }

    def drawEdges(edges: Seq[Edge]) = {
      if (edges.nonEmpty) {
        drawVertices(edges.head.p1 +: edges.map(_.p2))
      }
    }

    drawEdges _
  }
}

class SplineLevel(params: Params, world: World, start: Start) {
  import SplineTunnelPlatform._

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
      v
    })

  performCut(points, thickness, edgeDrawer(world))

}
