package game.one.core;

import com.github.davidmoten.rtree.Entry;
import com.github.davidmoten.rtree.RTree;
import com.github.davidmoten.rtree.geometry.Geometry;
import com.github.davidmoten.rtree.geometry.Line;
import rx.Observable;
import rx.functions.Func2;

/**
 * Created by martonpapp on 24/07/16.
 */
public class RTreeWrap {

    public static final <T> Observable<Entry<T, Line>> searchLine(
            RTree<T, Line> rtree,
            Line line,
            double maxDistance,
            Func2<Line, Line, Double> distance
    ) {
        return rtree.search(
                line,
                maxDistance,
                distance
        );
    }

}
