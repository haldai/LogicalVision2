/* Sampling library for prolog
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

#include "../img2/sampler.hpp"
#include "../utils2/memread.hpp"
#include "../utils2/errors.hpp"
#include "../utils2/utils.hpp"

#include <opencv2/core/core.hpp>
#include <opencv2/videoio/videoio.hpp>
#include <opencv2/highgui/highgui.hpp>
#include <SWI-cpp.h>
#include <SWI-Prolog.h>

/* sample_point(IMGSEQ, [X, Y, Z], VAR)
 * get variation of local area of point [X, Y, Z] in image sequence IMGSEQ
 */
PREDICATE(sample_point, 3) {
    char *p1 = (char*) A1;
    vector<int> vec = list2vec<int>(A2, 3);
    Scalar point(vec[0], vec[1], vec[2]); // coordinates scalar

    // get image sequence and compute variance
    const string add_seq(p1);
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    double var = cv_img_var_loc(seq, point);

    // return variance
    return A3 = PlTerm(var);
}

/* sample_point(IMGSEQ, [X, Y, Z], [RX, RY, RZ], VAR)
 * get variation of radius [RX, RY, RZ] local area
 * of point [X, Y, Z] in image sequence IMGSEQ
 */
PREDICATE(sample_point, 4) {
    char *p1 = (char*) A1;
    vector<int> vec = list2vec<int>(A2, 3);
    Scalar point(vec[0], vec[1], vec[2]); // coordinates scalar
    
    vector<int> r_vec = list2vec<int>(A3, 3);
    Scalar rad(r_vec[0], r_vec[1], r_vec[2]); // radius of local area

    // get image sequence and compute variance
    const string add_seq(p1);
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    double var = cv_img_var_loc(seq, point, rad);

    // return variance
    return A4 = PlTerm(var);
}

/* line_points(POINT, DIR, PTS)
 * get a list of points that on line
 * @POINT = [X, Y, Z]: a point that the line crosses
 * @DIR = [DX, DY, DZ]: direction of the line
 * @BOUND = [W, H, D]: size limit of the video (width, height and duration),
 *                     usually obained from 'size_3d(VID, W, H, D)'
 * @PTS: returned point list
 */
PREDICATE(line_points, 4) {
    // coordinates scalar
    vector<int> pt_vec = list2vec<int>(A1, 3);
    Scalar pt(pt_vec[0], pt_vec[1], pt_vec[2]);
    // direction scalar
    vector<int> dr_vec = list2vec<int>(A2, 3);
    Scalar dir(dr_vec[0], dr_vec[1], dr_vec[2]);
    // boundary scalar
    vector<int> bd_vec = list2vec<int>(A3, 3);
    Scalar bound(bd_vec[0], bd_vec[1], bd_vec[2]);
    // get points
    vector<Scalar> pts = get_line_points(pt, dir, bound);
    return A4 = point_vec2list(pts);
}

/* line_seg_points(START, END, PTS)
 * get a list of points that on line segment [START, END]
 * @START = [X1, Y1, Z1]: start point of the line segment
 * @END = [X2, Y2, Z2]: end point of the line segment
 * @BOUND = [W, H, D]: size limit of the video (width, height and duration),
 *                     usually obained from 'size_3d(VID, W, H, D)'
 * @PTS: returned point list
 */
PREDICATE(line_seg_points, 4) {
    // coordinates scalar
    vector<int> s_vec = list2vec<int>(A1, 3);
    Scalar start(s_vec[0], s_vec[1], s_vec[2]);
    // direction scalar
    vector<int> e_vec = list2vec<int>(A2, 3);
    Scalar end(e_vec[0], e_vec[1], e_vec[2]);
    // boundary scalar
    vector<int> bd_vec = list2vec<int>(A3, 3);
    Scalar bound(bd_vec[0], bd_vec[1], bd_vec[2]);
    // get points
    vector<Scalar> pts = get_line_seg_points(start, end, bound);
    return A4 = point_vec2list(pts);
}
/* ellipse_points(CENTRE, PARAM, BOUND, PTS)
 * get a list of points lie on a ellipse on a plane (the 3rd dimenstion
 * is fixed)
 * @CENTRE = [X, Y, _]: centre point of the ellipse
 * @PARAM = [A, B, ALPHA]: axis length (A >= B) and tilt angle (ALPHA)
 *                         of the ellipse
 * @BOUND = [W, H, D]: size limit of the video (width, height and duration),
 *                     usually obained from 'size_3d(VID, W, H, D)'
 * @PTS: returned point list
 */
PREDICATE(ellipse_points, 4) {
    // centre coordinate scalar
    vector<int> c_vec = list2vec<int>(A1, 3);
    Scalar centre(c_vec[0], c_vec[1], c_vec[2]);
    // parameter scalar
    vector<int> p_vec = list2vec<int>(A2, 3);
    Scalar param(p_vec[0], p_vec[1], p_vec[2]);
    // boundary scalar
    vector<int> bd_vec = list2vec<int>(A3, 3);
    Scalar bound(bd_vec[0], bd_vec[1], bd_vec[2]);
    // get points
    vector<Scalar> pts = get_ellipse_points(centre, param, bound);
    return A4 = point_vec2list(pts);
}

/* sample_line(IMGSEQ, [PX, PY, PZ], [A, B, C], T_VAR, P_LIST)
 *     equation of the line to be sampled:
 *         (X-PX)/A=(Y-PY)/B=(Z-PZ)/C
 * @(PX, PY, PZ) is a point that it crossed
 * @(A, B, C) is the direction of the line
 * @T_VAR is the threshold of local variance
 * @P_LIST is the returned points that exceed the variance threshold on the
 *    line
 */
PREDICATE(sample_line, 5) {
    char *p1 = (char*) A1;
    // coordinates scalar
    vector<int> pt_vec = list2vec<int>(A2, 3);
    Scalar pt(pt_vec[0], pt_vec[1], pt_vec[2]);
    // direction scalar
    vector<int> dr_vec = list2vec<int>(A3, 3);
    Scalar dir(dr_vec[0], dr_vec[1], dr_vec[2]);
    // get image sequence and compute variance
    const string add_seq(p1);
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    // get threshold
    double thresh = (double) A4;

    // sample a line and get all points that have high variance
    vector<Scalar> points = cv_sample_line(seq, pt, dir, thresh);
    return A5 = point_vec2list(points);
}

/* sample_line(IMGSEQ, [PX, PY, PZ], [A, B, C], T_VAR, P_LIST)
 *     equation of the line to be sampled:
 *         (X-PX)/A=(Y-PY)/B=(Z-PZ)/C
 * @[SX, SY, SZ] is starting point of the line segment
 * @[EX, EY, EZ] is ending point of the line segment
 * @T_VAR is the threshold of local variance
 * @P_LIST is the returned points that exceed the variance threshold on the
 *    line
 */
PREDICATE(sample_line_seg, 5) {
    char *p1 = (char*) A1;
    // coordinates scalar
    vector<int> pt_vec = list2vec<int>(A2, 3);
    Scalar pt(pt_vec[0], pt_vec[1], pt_vec[2]);
    // direction scalar
    vector<int> dr_vec = list2vec<int>(A3, 3);
    Scalar dir(dr_vec[0], dr_vec[1], dr_vec[2]);
    // get image sequence and compute variance
    const string add_seq(p1);
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    // get threshold
    double thresh = (double) A4;
    
    // sample a line and get all points that have high variance
    vector<Scalar> points = cv_sample_line_seg(seq, pt, dir, thresh);
    return A5 = point_vec2list(points);
}

/* sample_line(IMGSEQ, [PX, PY, PZ], [A, B, C], [RX, RY, RZ], T_VAR, P_LIST)
 *     equation of the line to be sampled:
 *         (X-PX)/A=(Y-PY)/B=(Z-PZ)/C
 * @[PX, PY, PZ] is a point that it crossed
 * @[A, B, C] is the direction of the line
 * @[RX, RY, RZ] is the radius of variance evaluator
 * @T_VAR is the threshold of local variance
 * @P_LIST is the returned points that exceed the variance threshold on the
 *    line
 */
PREDICATE(sample_line, 6) {
    char *p1 = (char*) A1;
    // coordinates scalar
    vector<int> pt_vec = list2vec<int>(A2, 3);
    Scalar pt(pt_vec[0], pt_vec[1], pt_vec[2]);
    // direction scalar
    vector<int> dr_vec = list2vec<int>(A3, 3);
    Scalar dir(dr_vec[0], dr_vec[1], dr_vec[2]);
    // radius scalar
    vector<int> r_vec = list2vec<int>(A4, 3);
    Scalar rad(r_vec[0], r_vec[1], r_vec[2]);
    // get image sequence and compute variance
    const string add_seq(p1);
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    // get threshold
    double thresh = (double) A5;
    // sample a line and get all points that have high variance
    vector<Scalar> points = cv_sample_line(seq, pt, dir, thresh, rad);
    return A6 = point_vec2list(points);
}

/* sample_line(IMGSEQ, [PX, PY, PZ], [A, B, C], [RX, RY, RZ], T_VAR, P_LIST)
 *     equation of the line to be sampled:
 *         (X-PX)/A=(Y-PY)/B=(Z-PZ)/C
 * @[SX, SY, SZ] is starting point of the line segment
 * @[EX, EY, EZ] is ending point of the line segment
 * @[RX, RY, RZ] is the radius of variance evaluator 
 * @T_VAR is the threshold of local variance
 * @P_LIST is the returned points that exceed the variance threshold on the
 *    line
 */
PREDICATE(sample_line_seg, 6) {
    char *p1 = (char*) A1;
    // coordinates scalar
    vector<int> pt_vec = list2vec<int>(A2, 3);
    Scalar pt(pt_vec[0], pt_vec[1], pt_vec[2]);
    // direction scalar
    vector<int> dr_vec = list2vec<int>(A3, 3);
    Scalar dir(dr_vec[0], dr_vec[1], dr_vec[2]);
    // radius scalar
    vector<int> r_vec = list2vec<int>(A4, 3);
    Scalar rad(r_vec[0], r_vec[1], r_vec[2]);
    // get image sequence and compute variance
    const string add_seq(p1);
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    // get threshold
    double thresh = (double) A5;
    // sample a line and get all points that have high variance
    vector<Scalar> points = cv_sample_line_seg(seq, pt, dir, thresh, rad);
    return A6 = point_vec2list(points);
}
