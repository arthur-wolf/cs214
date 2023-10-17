package poly

import poly.MyList.*

type NodeId = Int
type DirectedEdge = (NodeId, NodeId)
type DirectedGraph = MyList[DirectedEdge]
type Triangle = (NodeId, NodeId, NodeId)

def triangles(edges: DirectedGraph): MyList[Triangle] =
  flatMap[DirectedEdge, Triangle](e0 =>
    e0 match
      case (a, b) if a < b =>
        flatMap[DirectedEdge, Triangle](e1 =>
          e1 match
            case (`b`, c) if a < c =>
              map(_ => (a, b, c))(filter[DirectedEdge](e3 =>
                e3 match
                  case (`c`, `a`) => true
                  case _          => false
              )(edges))
            case _ => Nil
        )(edges)
      case _ => Nil
  )(edges)