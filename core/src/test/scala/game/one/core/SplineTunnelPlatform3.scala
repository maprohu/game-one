package game.one.core

import java.awt.geom.Line2D

import com.badlogic.gdx.math._
import com.badlogic.gdx.physics.box2d.World
import com.github.davidmoten.rtree.{Entry, RTree}
import com.github.davidmoten.rtree.geometry.{Geometry, Line, Point, Rectangle}
import game.one.core.GameOne.Level
import game.one.core.SplineTunnelPlatform3.Params
import rx.functions.Func2

import scala.collection.JavaConversions._
import scala.collection.immutable._

/**
  * Created by martonpapp on 23/07/16.
  */
object SplineTunnelPlatform3 {

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
    new SplineLevel3(params, world, start)
  }

  def test : Level = { (world, start) =>
    performCut(
      Seq(
        new Vector2(-1, 0),
        new Vector2(1, 0),
        new Vector2(0, 1)
      ).map(v => (v.add(start.origin), 0f)),
      _ => 0.2f,
      _ => 0.2f,
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
    points: Seq[(Vector2, Float)],
    dist1: Float => Float,
    dist2: Float => Float,
    drawEdges: Seq[Edge] => Unit
  ) = {

    val (sc1, sc2) =
      (points :+ points.head)
        .sliding(2)
        .collect({
          case Seq((p1, t), (p2, _)) =>
            val n =
              new Vector2(p2)
                .sub(p1)
                .nor()
                .rotate90(1)

            val ml = Line.create(p1.x, p1.y, p2.x, p2.y)

            val d1 = dist1(t)
            val e1p1 = new Vector2(p1).mulAdd(n, d1)
            val e1p2 = new Vector2(p2).mulAdd(n, d1)

            val pol1 = new Polygon(
              Array(
                p1.x, p1.y,
                p2.x, p2.y,
                e1p2.x, e1p2.y,
                e1p1.x, e1p1.y
              )
            )
            val c1 = new Cutter(pol1)

            val d2 = dist2(t)
            val e2p1 = new Vector2(p1).mulAdd(n, -d2)
            val e2p2 = new Vector2(p2).mulAdd(n, -d2)

            val pol2 = new Polygon(
              Array(
                p1.x, p1.y,
                p2.x, p2.y,
                e2p2.x, e2p2.y,
                e2p1.x, e2p1.y
              ).grouped(2).toSeq.reverse.flatten.toArray
            )
            val c2 = new Cutter(pol2)

            (
              (
                c1,
                Edge(
                  Vertex(e1p1, c1),
                  Vertex(e1p2, c1)
                )
              ),
              (
                c2,
                Edge(
                  Vertex(e2p1, c2),
                  Vertex(e2p2, c2)
                )
              )
            )
        })
        .toSeq
        .unzip

    val (c1s, e1s) = sc1.unzip
    val (c2s, e2s) = sc2.unzip

    case class Half(
      cutters: Seq[Cutter],
      edges: Seq[Edge]
    ) {
      val connectedEdges =
        (edges :+ edges.head)
          .sliding(2)
          .collect({
            case Seq(e1, e2) =>
              Seq(
                e1,
                Edge(
                  e1.p2,
                  e2.p1
                )
              )
          })
          .flatten
          .toList
    }

    val halves = Seq(
      Half(c1s.toList, e1s.toList),
      Half(c2s.toList, e2s.toList)
    )

    val allCutters = halves.flatMap(_.cutters)

    val cutterTree =
      RTree
        .create[Cutter, Cutter]()
        .add(
          asJavaIterable(
            allCutters.map({ cutter =>
              Entry.entry(cutter, cutter)
            })
          )
        )

    val edgeTree =
      RTree
        .create[Edge, Line]()
        .add(
          asJavaIterable(
            halves.flatMap(_.connectedEdges).map({ edge =>
              Entry.entry(edge, edge.line)
            })
          )
        )

    halves.foreach { half =>
      import half._



      val refinedEdges = connectedEdges.flatMap({ edge =>
        val edgeCuts = edge.cutters

        val realCutters = edgeTree.search(edge.line).toBlocking.toIterable
          .filterNot({ entry =>
            val found = entry.value()

            (found eq edge) ||
              (found.p1 eq edge.p2) ||
              (found.p2 eq edge.p1)
          })
          .flatMap({ cuttingEdge =>
            val is = new Vector2()
            if (Intersector.intersectSegments(
              edge.line.x1(),
              edge.line.y1(),
              edge.line.x2(),
              edge.line.y2(),
              cuttingEdge.geometry().x1(),
              cuttingEdge.geometry().y1(),
              cuttingEdge.geometry().x2(),
              cuttingEdge.geometry().y2(),
              is
            )) {
              Some((is.dst2(edge.p1.v), is, cuttingEdge.value()))
            } else {
              None
            }
          })
          .toSeq
          .sortBy(_._1)

        if (realCutters.isEmpty) {
          Seq(edge)
        } else {
          val (lastVertex, cutEdges) = realCutters
            .foldLeft((edge.p1, Seq.empty[Edge]))({ (acc, item) =>
              val (p1, items) = acc
              val (_, p2, cutterEdge) = item

              val vertex = new Vertex(
                p2,
                edgeCuts ++ cutterEdge.cutters
              )

              val edge = Edge(
                p1,
                vertex
              )

              (
                vertex,
                items :+ edge
                )
            })

          cutEdges :+ Edge(lastVertex, edge.p2)
        }

      })
      .to[Seq]

      val cutterIntersectsLine = new Func2[Cutter, Point, java.lang.Boolean] {
        override def call(t1: Cutter, t2: Point): java.lang.Boolean = {
          t1.isInside(t2.x(), t2.y())
        }
      }

      def shouldCutVertex(vertex: Vertex) : Boolean = {
        cutterTree.search(
          vertex.point,
          cutterIntersectsLine
        ).toBlocking.toIterable
          .exists({ c =>
            !vertex.cutters.contains(c.value)
          })
      }

      def shouldCutEdge(edge: Edge) : Boolean = {
        shouldCutVertex(edge.p1) || shouldCutVertex(edge.p2)
      }

      def removeCut(edges: Seq[Edge]) : (Seq[Edge], Seq[Edge]) = {
        val (_, left) = edges.span({ edge =>
          shouldCutEdge(edge)
        })
        left.span({ edge =>
          !shouldCutEdge(edge)
        })
      }

      def cutStream(edges: Seq[Edge]) : Stream[Seq[Edge]] = {
        if (edges.isEmpty) Stream.empty[Seq[Edge]]
        else {
          val (keep, left) = removeCut(edges)
          Stream.cons(keep, cutStream(left))
        }
      }

      cutStream(refinedEdges)
        .foreach(drawEdges)

//      drawEdges(refinedEdges)
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

class SplineLevel3(params: Params, world: World, start: Start) {
  import SplineTunnelPlatform3._

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
      (v, t)
    })

  performCut(points, dist, dist, edgeDrawer(world))

}
