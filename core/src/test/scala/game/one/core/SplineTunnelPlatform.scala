package game.one.core

import java.awt.geom.Line2D

import com.badlogic.gdx.math._
import com.badlogic.gdx.physics.box2d.World
import com.github.davidmoten.rtree.{Entry, RTree}
import com.github.davidmoten.rtree.geometry.{Geometry, Line, Point, Rectangle}
import game.one.core.GameOne.Level
import game.one.core.SplineTunnelPlatform.Params
import rx.functions.Func2

import scala.collection.JavaConversions._

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
      ).map(v => (v.add(start.origin), 0.2f)),
      _ => 1f,
      edgeDrawer(world)
    )
  }


  // TODO: different cutter for each side
  class Cutter(
    line: Line,
    d: Float
  ) extends Geometry {

    val rectangle = Rectangle.create(
      Math.min(
        line.x1(),
        line.x2()
      ) - d,
      Math.min(
        line.y1(),
        line.y2()
      ) - d,
      Math.max(
        line.x1(),
        line.x2()
      ) + d,
      Math.max(
        line.y1(),
        line.y2()
      ) + d
    )

    def isInside(x: Float, y: Float) : Boolean = {
      Line2D.ptSegDistSq(
        line.x1(),
        line.y1(),
        line.x2(),
        line.y2(),
        x,
        y
      ) < d*d
    }

    override def distance(r: Rectangle): Double = ???

    override def intersects(r: Rectangle): Boolean = {
      (line.distance(r) < d).asInstanceOf[Boolean]
    }

    override def mbr(): Rectangle = rectangle
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
    def cutters = p1.cutters intersect p2.cutters
  }


  case class Half(
    cutters: Seq[Cutter],
    edges: Seq[Edge]
  )

  def performCut(points: Seq[(Vector2, Float)], dist: Float => Float, drawEdges: Seq[Edge] => Unit) = {

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
            val d = dist(t)
            val cutter = new Cutter(ml, d)

            (
              (
                cutter,
                Edge(
                  Vertex(new Vector2(p1).mulAdd(n, d), cutter),
                  Vertex(new Vector2(p2).mulAdd(n, d), cutter)
                )
                ),
              (
                cutter,
                Edge(
                  Vertex(new Vector2(p1).mulAdd(n, -d), cutter),
                  Vertex(new Vector2(p2).mulAdd(n, -d), cutter)
                )
                )
              )

        })
        .toSeq
        .unzip

    val (c1s, e1s) = sc1.unzip
    val (c2s, e2s) = sc2.unzip

    val halves = Seq(
      Half(c1s.toList, e1s.toList),
      Half(c2s.toList, e2s.toList)
    )

    halves.foreach { half =>
      import half._


      val edgeTree =
        RTree
          .create[Edge, Line]()
          .add(
            asJavaIterable(
              edges.map({ edge =>
                Entry.entry(edge, edge.line)
              })
            )
          )

      val refinedEdges = edges.flatMap({ edge =>
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

      val cutterTree =
        RTree
          .create[Cutter, Cutter]()
          .add(
            asJavaIterable(
              cutters.map({ cutter =>
                Entry.entry(cutter, cutter)
              })
            )
          )

      val cutterIntersectsLine = new Func2[Cutter, Line, java.lang.Boolean] {
        override def call(t1: Cutter, t2: Line): java.lang.Boolean = {
          t1.isInside(t2.x1(), t2.y1()) ||
            t1.isInside(t2.x2(), t2.y2())
        }
      }

      def shouldCut(edge: Edge) = {
        cutterTree.search(
          edge.line,
          cutterIntersectsLine
        ).toBlocking.toIterable
          .exists(c => !edge.cutters.contains(c))
      }

      def removeCut(edges: Seq[Edge]) : (Seq[Edge], Seq[Edge]) = {
        val (_, left) = edges.span({ edge =>
          shouldCut(edge)
        })
        left.span({ edge =>
          !shouldCut(edge)
        })
      }

//      Stream.iterate((Seq.empty[Edge], refinedEdges))(a => removeCut(a._2))
//        .drop(1)
//        .map(_._1)
//        .takeWhile(_.nonEmpty)
//        .toList
//        .foreach(drawEdges)

      drawEdges(refinedEdges)
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
      edges.headOption.foreach { head =>
        drawVertices(head.p1 +: edges.tail.map(_.p2))
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
      (v, t)
    })

  performCut(points, dist, edgeDrawer(world))

}
