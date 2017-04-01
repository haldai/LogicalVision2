/*************************************************************************
This file is part of Logical Vision 2.

Logical Vision 2 is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Logical Vision 2 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Logical Vision 2.  If not, see <http://www.gnu.org/licenses/>.
************************************************************************/
/* Sampling library for prolog
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

#include "sampler.hpp"
#include "memread.hpp"
#include "errors.hpp"
#include "utils.hpp"

#include <opencv2/core/core.hpp>
#include <opencv2/videoio/videoio.hpp>
#include <opencv2/highgui/highgui.hpp>
#include <SWI-cpp.h>
#include <SWI-Prolog.h>
#include <cmath>

/* sample_point_var(IMGSEQ, [X, Y, Z], VAR)
 * get variation of local area of point [X, Y, Z] in image sequence IMGSEQ
 */
PREDICATE(sample_point_var, 3) {
    char *p1 = (char*) A1;
    vector<int> vec = list2vec<int>(A2, 3);
    Scalar point(vec[0], vec[1], vec[2]); // coordinates scalar

    // get image sequence and compute variance
    const string add_seq(p1);
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    double var = cv_imgs_point_var_loc(seq, point);

    // return variance
    return A3 = PlTerm(var);
}

/* sample_point_var(IMGSEQ, [X, Y, Z], [RX, RY, RZ], VAR)
 * get variation of radius [RX, RY, RZ] local area
 * of point [X, Y, Z] in image sequence IMGSEQ
 */
PREDICATE(sample_point_var, 4) {
    char *p1 = (char*) A1;
    vector<int> vec = list2vec<int>(A2, 3);
    Scalar point(vec[0], vec[1], vec[2]); // coordinates scalar
    
    vector<int> r_vec = list2vec<int>(A3, 3);
    Scalar rad(r_vec[0], r_vec[1], r_vec[2]); // radius of local area

    // get image sequence and compute variance
    const string add_seq(p1);
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    double var = cv_imgs_point_var_loc(seq, point, rad);

    // return variance
    return A4 = PlTerm(var);
}

/* sample_point_scharr(IMGSEQ, [X, Y, Z], GRAD)
 * get scharr gradient of point [X, Y, Z] in image sequence IMGSEQ
 */
PREDICATE(sample_point_scharr, 3) {
    char *p1 = (char*) A1;
    vector<int> vec = list2vec<int>(A2, 3);
    Scalar point(vec[0], vec[1], vec[2]); // coordinates scalar

    // get image sequence and compute variance
    const string add_seq(p1);
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    double var = cv_imgs_point_scharr(seq, point);

    // return variance
    return A3 = PlTerm(var);
}

/* sample_point_scharr_2d(IMG, [X, Y], GRAD)
 * get scharr gradient of point [X, Y] in image IMG
 */
PREDICATE(sample_point_scharr_2d, 3) {
    char *p1 = (char*) A1;
    vector<int> vec = list2vec<int>(A2, 2);
    Scalar point(vec[0], vec[1], 0.0); // coordinates scalar

    // get image sequence and compute variance
    const string add_img(p1);
    Mat *img = str2ptr<Mat>(add_img);
    double var = cv_img_point_scharr(img, point);

    // return variance
    return A3 = PlTerm(var);
}

/* point_color(IMGSEQ, [X, Y, Z], COLOR)
 * get LAB color of local area of point [X, Y, Z] in image sequence IMGSEQ
 */
PREDICATE(point_color, 3) {
    char *p1 = (char*) A1;
    vector<int> vec = list2vec<int>(A2, 3);
    Scalar point(vec[0], vec[1], vec[2]); // coordinates scalar

    // get image sequence and compute variance
    const string add_seq(p1);
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    Scalar col = cv_imgs_point_color_loc(seq, point);
    vector<double> col_vec = {col[0], col[1], col[2]};

    // return colors
    return A3 = vec2list<double>(col_vec);
}

/* point_color(IMGSEQ, [X, Y, Z], [RX, RY, RZ], COLOR)
 * get average LAB color of point's neigborhood with radius [RX, RY, RZ]
 * local area
 */
PREDICATE(point_color, 4) {
    char *p1 = (char*) A1;
    vector<int> vec = list2vec<int>(A2, 3);
    Scalar point(vec[0], vec[1], vec[2]); // coordinates scalar
    
    vector<int> r_vec = list2vec<int>(A3, 3);
    Scalar rad(r_vec[0], r_vec[1], r_vec[2]); // radius of local area

    // get image sequence and compute variance
    const string add_seq(p1);
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    Scalar col = cv_imgs_point_color_loc(seq, point, rad);
    vector<double> col_vec = {col[0], col[1], col[2]};

    // return variance
    return A4 = vec2list<double>(col_vec);
}

/* point_color_2d(IMG, [X, Y], COLOR)
 * get LAB color of local area of point [X, Y] in an image IMG
 */
PREDICATE(point_color_2d, 3) {
    char *p1 = (char*) A1;
    vector<int> vec = list2vec<int>(A2, 2);
    Scalar point(vec[0], vec[1], 0); // coordinates scalar

    // get image sequence and compute variance
    const string add_img(p1);
    Mat *img = str2ptr<Mat>(add_img);
    Scalar col = cv_img_point_color_loc_2d(img, point);
    vector<double> col_vec = {col[0], col[1], col[2]};

    // return colors
    return A3 = vec2list<double>(col_vec);
}

/* line_points(+POINT, +DIR, +BOUND, -PTS)
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
    vector<double> dr_vec = list2vec<double>(A2, 3);
    Scalar dir(dr_vec[0], dr_vec[1], dr_vec[2]);
    // boundary scalar
    vector<int> bd_vec = list2vec<int>(A3, 3);
    Scalar bound(bd_vec[0], bd_vec[1], bd_vec[2]);
    // get points
    vector<Scalar> pts = get_line_points(pt, dir, bound);
    return A4 = point_vec2list(pts);
}

/* line_points(+POINT, +DIR, +TOPLEFT, +BOTTOMRIGHT, -PTS)
 * get a list of points that on line
 * @POINT = [X, Y, Z]: a point that the line crosses
 * @DIR = [DX, DY, DZ]: direction of the line
 * @TOPLEFT/BOTTOMRIGHT = [W, H, D]: size limit of the range (width,
 *                     height and duration)
 * @PTS: returned point list
 */
PREDICATE(line_points, 5) {
    // coordinates scalar
    vector<int> pt_vec = list2vec<int>(A1, 3);
    Scalar pt(pt_vec[0], pt_vec[1], pt_vec[2]);
    // direction scalar
    vector<double> dr_vec = list2vec<double>(A2, 3);
    Scalar dir(dr_vec[0], dr_vec[1], dr_vec[2]);
    // boundary scalar
    vector<int> bd1_vec = list2vec<int>(A3, 3);
    Scalar tl(bd1_vec[0], bd1_vec[1], bd1_vec[2]);
    vector<int> bd2_vec = list2vec<int>(A4, 3);
    Scalar br(bd2_vec[0], bd2_vec[1], bd2_vec[2]);
    // get points
    vector<Scalar> pts = get_line_points(pt, dir, tl, br);
    return A5 = point_vec2list(pts);
}

/* ray_points(+POINT, +DIR, +BOUND, -PTS)
 * get a list of points that on line
 * @POINT = [X, Y, Z]: starting point of the ray
 * @DIR = [DX, DY, DZ]: direction of the ray
 * @BOUND = [W, H, D]: size limit of the video (width, height and duration),
 *                     usually obained from 'size_3d(VID, W, H, D)'
 * @PTS: returned point list
 */
PREDICATE(ray_points, 4) {
    try {
        // coordinates scalar        
        vector<int> pt_vec = list2vec<int>(A1, 3);
        // direction scalar        
        vector<double> dr_vec = list2vec<double>(A2, 3);
        if (pt_vec.size() < 3 || dr_vec.size() < 3)
            return FALSE;
        Scalar pt(pt_vec[0], pt_vec[1], pt_vec[2]);
        Scalar dir(dr_vec[0], dr_vec[1], dr_vec[2]);
        // boundary scalar
        vector<int> bd_vec = list2vec<int>(A3, 3);
        Scalar bound(bd_vec[0], bd_vec[1], bd_vec[2]);
        // get points
        vector<Scalar> pts = get_ray_points(pt, dir, bound);
        return A4 = point_vec2list(pts);
    } catch (...) {
        return FALSE;
    }
}

/* line_seg_points(START, END, BOUND, PTS)
 * get a list of points that on line segment [START, END]
 * @START = [X1, Y1, Z1]: start point of the line segment
 * @END = [X2, Y2, Z2]: end point of the line segment
 * @BOUND = [W, H, D]: size limit of the video (width, height and duration),
 *                     usually obained from 'size_3d(VID, W, H, D)'
 * @PTS: returned point list
 */
PREDICATE(line_seg_points, 4) {
    // start scalar
    vector<int> s_vec = list2vec<int>(A1, 3);
    Scalar start(s_vec[0], s_vec[1], s_vec[2]);
    // end scalar
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
 * get a list of points lie on an ellipse on a plane (the 3rd dimenstion
 * is fixed)
 * @CENTRE = [X, Y, _]: centre point of the ellipse
 * @PARAM = [A, B, ALPHA]: axis length (A >= B) and tilt angle (ALPHA)
 *      of the ellipse.
 * !!The unit of angle is DEG, not RAD; smaller than 1 then random angle!!
 * @BOUND = [W, H, D]: size limit of the video (width, height and duration),
 *      usually obained from 'size_3d(VID, W, H, D)'
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

/* circle_points(+Center, +Radius, +BOUND, -PTS)
 * @Center =[X, Y, Frame]: Center
 * @Radius: the radius
 * @BOUND = [W, H, D]: size limit of the video (width, height and duration),
 *      usually obained from 'size_3d(VID, W, H, D)'
 * @PTS: returned point list
 */
PREDICATE(circle_points, 4) {
    // parameter coordinate scalar
    vector<int> c_vec = list2vec<int>(A1, 3);
    int rad = (int) A2;
    // boundary scalar
    vector<int> bd_vec = list2vec<int>(A3, 3);
    Scalar bound(bd_vec[0], bd_vec[1], bd_vec[2]);
    // get points
    Scalar param(c_vec[0], c_vec[1], c_vec[2], rad);
    vector<Scalar> pts = get_circle_points(param, bound);
    return A4 = point_vec2list(pts);
}

/* in_cube_points(+CENTRE, +RADIUS, +BOUND, -PTS)
 * get a list of points in side of a cube
 * @CENTRE = [X, Y, Z]: centre point of the cube
 * @RADIUS = [A, B, C]: radius of the cube
 * @BOUND = [W, H, D]: size limit of the video (width, height and duration),
 *      usually obained from 'size_3d(VID, W, H, D)'
 * @PTS: returned point list
 */
PREDICATE(in_cube_points, 4) {
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
    vector<Scalar> pts = get_in_cube_points(centre, param, bound);
    return A4 = point_vec2list(pts);
}
/* dist_point_elips_2d(+POINT, +CEN, +PARAM, -DIST, -CLOSEST)
 * @POINT: [X, Y], the input point
 * @CEN: [CX, CY], center of the ellipse
 * @PARAM: [A, B, ALPHA], axes of ellipse and the tilt angle
 * @DIST: distance between the point and the ellipse
 * @CLOSEST: the closest point on ellipse to [X, Y]
 */
PREDICATE(dist_point_elps_2d, 5) {
    // read in
    vector<int> p_vec = list2vec<int>(A1, 2); // point
    vector<int> c_vec = list2vec<int>(A2, 2); // center of ellipse
    vector<double> para_vec = list2vec<double>(A3, 3); // parameter of ellipse
    cv::Point2i point(p_vec[0], p_vec[1]);
    cv::Point2i center(c_vec[0], c_vec[1]);
    Scalar param(para_vec[0], para_vec[1], para_vec[2], 0.0);
    cv::Point2i closest;
    try {
        double dist = DistancePointEllipse(point, center, param, closest);
        A4 = PlTerm(dist);
        vector<long> clst = {(long) closest.x, (long) closest.y};
        A5 = vec2list<long>(clst);
    } catch (...) {
        cout << "Cannot compute the distance!" << endl;
        return FALSE;
    }
    return TRUE;
}

/* pts_var(+IMGSEQ, +PTS, -VARS)
 * For a list of points, return their variance
 * @IMGSEQ: input images
 * @PTS: point list, [[X1, Y1, Z1], ...]
 * @VARS: variances of each point, [V1, ...]
 */
PREDICATE(pts_var, 3) {
    // image sequence
    char *p1 = (char*) A1;
    const string add_seq(p1);    
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    // point list
    vector<Scalar> pts = point_list2vec(A2);
    // calculate variances
    vector<double> vars = cv_imgs_points_var_loc(seq, pts);
    return A3 = vec2list(vars);
}

/* pts_set_var(+IMGSEQ, +PTS, -VAR)
 * Compute the variance of a set of poins
 * @IMGSEQ: input images
 * @PTS: point list, [[X1, Y1, Z1], ...]
 * @VAR: variances of the point set, [V1, ...]
 */
PREDICATE(pts_set_var, 3) {
    // image sequence
    char *p1 = (char*) A1;
    const string add_seq(p1);    
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    // point list
    vector<Scalar> pts = point_list2vec(A2);
    // calculate variances
    double var = cv_imgs_points_var(seq, pts);
    return A3 = var;
}

/* pts_scharr(+IMGSEQ, +PTS, -VARS)
 * For a list of points, return their scharr gradients
 * @IMGSEQ: input images
 * @PTS: point list, [[X1, Y1, Z1], ...]
 * @GRADS: gradients of each point, [G1, ...]
 */
PREDICATE(pts_scharr, 3) {
    // image sequence
    char *p1 = (char*) A1;
    const string add_seq(p1);    
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    // point list
    vector<Scalar> pts = point_list2vec(A2);
    // calculate variances
    vector<double> vars = cv_imgs_points_scharr(seq, pts);
    return A3 = vec2list(vars);
}

/* pts_scharr_2d(+IMG, +PTS, -VARS)
 * For a list of points, return their scharr gradients
 * @IMGSEQ: input images
 * @PTS: point list, [[X1, Y1, Z1], ...]
 * @GRADS: gradients of each point, [G1, ...]
 */
PREDICATE(pts_scharr_2d, 3) {
    // image sequence
    char *p1 = (char*) A1;
    const string add_seq(p1);    
    Mat *img = str2ptr<Mat>(add_seq);
    // point list
    vector<Scalar> pts = point_list2vec(A2);
    // calculate variances
    vector<double> vars = cv_img_points_scharr(img, pts);
    return A3 = vec2list(vars);
}


/* pts_color(+IMGSEQ, +PTS, -COLORS)
 * For a list of points, return their color
 * @IMGSEQ: input images
 * @PTS: point list, [[X1, Y1, Z1], ...]
 * @VARS: color of each point, [[L,A,B], ...]
 */
PREDICATE(pts_color, 3) {
    // image sequence
    char *p1 = (char*) A1;
    const string add_seq(p1);    
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    // point list
    vector<Scalar> pts = point_list2vec(A2, -1, 2);
    // calculate variances
    vector<Scalar> colors = cv_imgs_points_color_loc(seq, pts);
    return A3 = scalar_vec2list<double>(colors);
}

/* pts_color_2d(+IMG, +PTS, -COLORS)
 * For a list of points, return their color
 * @IMGSEQ: input image
 * @PTS: point list, [[X1, Y1], ...]
 * @VARS: color of each point, [[L,A,B], ...]
 */
PREDICATE(pts_color_2d, 3) {
    // image sequence
    char *p1 = (char*) A1;
    const string add_img(p1);    
    Mat *img = str2ptr<Mat>(add_img);
    // point list
    vector<Scalar> pts = point_list2vec(A2);
    // calculate variances
    vector<Scalar> colors = cv_img_points_color_loc_2d(img, pts);
    return A3 = scalar_vec2list<double>(colors);
}

/* pts_var_loc(+IMGSEQ, +PTS, +LOC, -VARS)
 * For a list of points, return their variance
 * @IMGSEQ: input images
 * @PTS: point list, [[X1, Y1, Z1], ...]
 * @LOC: local radius
 * @VARS: variances of each point, [V1, ...]
 */
PREDICATE(pts_var_loc, 4) {
    // image sequence
    char *p1 = (char*) A1;
    const string add_seq(p1);    
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    // point list
    vector<Scalar> pts = point_list2vec(A2);
    // radius
    vector<int> r_vec = list2vec<int>(A3, 3);
    Scalar rad(r_vec[0], r_vec[1], r_vec[2]);
    cout << rad << endl;
    // calculate variances
    vector<double> vars = cv_imgs_points_var_loc(seq, pts, rad);
    return A4 = vec2list(vars);
}

/* pts_color_loc(+IMGSEQ, +PTS, +LOC, -COLORS)
 * For a list of points, return their color
 * @IMGSEQ: input images
 * @PTS: point list, [[X1, Y1, Z1], ...]
 * @LOC: local radius (so we are getting localy averaged color...) 
 * @VARS: color of each point, [[L,A,B], ...]
 */
PREDICATE(pts_color_loc, 4) {
    // image sequence
    char *p1 = (char*) A1;
    const string add_seq(p1);    
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    // point list
    vector<Scalar> pts = point_list2vec(A2);
    // radius
    vector<int> r_vec = list2vec<int>(A3, 3);
    Scalar rad(r_vec[0], r_vec[1], r_vec[2]);
    // calculate variances
    vector<Scalar> colors = cv_imgs_points_color_loc(seq, pts, rad);
    return A4 = scalar_vec2list<double>(colors);
}

/* line_pts_var_geq_T(IMGSEQ, [PX, PY, PZ], [A, B, C], T_VAR, P_LIST)
 *     equation of the line to be sampled:
 *         (X-PX)/A=(Y-PY)/B=(Z-PZ)/C
 * @(PX, PY, PZ) is a point that it crossed
 * @(A, B, C) is the direction of the line
 * @T_VAR is the threshold of local variance
 * @P_LIST is the returned points that exceed the variance threshold on the
 *    line
 */
PREDICATE(line_pts_var_geq_T, 5) {
    char *p1 = (char*) A1;
    // coordinates scalar
    vector<int> pt_vec = list2vec<int>(A2, 3);
    Scalar pt(pt_vec[0], pt_vec[1], pt_vec[2]);
    // direction scalar
    vector<double> dr_vec = list2vec<double>(A3, 3);
    Scalar dir(dr_vec[0], dr_vec[1], dr_vec[2]);
    // get image sequence and compute variance
    const string add_seq(p1);
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    // get threshold
    double thresh = (double) A4;

    // sample a line and get all points that have high variance
    vector<Scalar> points = cv_line_pts_var_geq_T(seq, pt, dir, thresh);
    return A5 = point_vec2list(points);
}

/* line_seg_pts_var_geq_T(IMGSEQ, [SX, SY, SZ], [EX, EY, EZ], T_VAR, P_LIST)
 *     equation of the line to be sampled:
 *         (X-PX)/A=(Y-PY)/B=(Z-PZ)/C
 * @[SX, SY, SZ] is starting point of the line segment
 * @[EX, EY, EZ] is ending point of the line segment
 * @T_VAR is the threshold of local variance
 * @P_LIST is the returned points that exceed the variance threshold on the
 *    line
 */
PREDICATE(line_seg_pts_var_geq_T, 5) {
    char *p1 = (char*) A1;
    // start point scalar
    vector<int> st_vec = list2vec<int>(A2, 3);
    Scalar st(st_vec[0], st_vec[1], st_vec[2]);
    // end point scalar
    vector<int> ed_vec = list2vec<int>(A3, 3);
    Scalar ed(ed_vec[0], ed_vec[1], ed_vec[2]);
    // get image sequence and compute variance
    const string add_seq(p1);
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    // get threshold
    double thresh = (double) A4;
    
    // sample a line and get all points that have high variance
    vector<Scalar> points = cv_line_seg_pts_var_geq_T(seq, st, ed, thresh);
    return A5 = point_vec2list(points);
}

/* line_pts_var_geq_T(IMGSEQ, [PX, PY, PZ], [A, B, C], [RX, RY, RZ],
 *                    T_VAR, P_LIST)
 *     equation of the line to be sampled:
 *         (X-PX)/A=(Y-PY)/B=(Z-PZ)/C
 * @[PX, PY, PZ] is a point that it crossed
 * @[A, B, C] is the direction of the line
 * @[RX, RY, RZ] is the radius of variance evaluator
 * @T_VAR is the threshold of local variance
 * @P_LIST is the returned points that exceed the variance threshold on the
 *    line
 */
PREDICATE(line_pts_var_geq_T, 6) {
    char *p1 = (char*) A1;
    // coordinates scalar
    vector<int> pt_vec = list2vec<int>(A2, 3);
    Scalar pt(pt_vec[0], pt_vec[1], pt_vec[2]);
    // direction scalar
    vector<double> dr_vec = list2vec<double>(A3, 3);
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
    vector<Scalar> points = cv_line_pts_var_geq_T(seq, pt, dir, thresh, rad);
    return A6 = point_vec2list(points);
}

/* line_seg_pts_var_geq_T(IMGSEQ, [PX, PY, PZ], [A, B, C],
                          [RX, RY, RZ], T_VAR, P_LIST)
 *     equation of the line to be sampled:
 *         (X-PX)/A=(Y-PY)/B=(Z-PZ)/C
 * @[SX, SY, SZ] is starting point of the line segment
 * @[EX, EY, EZ] is ending point of the line segment
 * @[RX, RY, RZ] is the radius of variance evaluator 
 * @T_VAR is the threshold of local variance
 * @P_LIST is the returned points that exceed the variance threshold on the
 *    line
 */
PREDICATE(line_seg_pts_var_geq_T, 6) {
    char *p1 = (char*) A1;
    // start point scalar
    vector<int> st_vec = list2vec<int>(A2, 3);
    Scalar st(st_vec[0], st_vec[1], st_vec[2]);
    // end point scalar
    vector<int> ed_vec = list2vec<int>(A3, 3);
    Scalar ed(ed_vec[0], ed_vec[1], ed_vec[2]);
    // radius scalar
    vector<int> r_vec = list2vec<int>(A4, 3);
    Scalar rad(r_vec[0], r_vec[1], r_vec[2]);
    // get image sequence and compute variance
    const string add_seq(p1);
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    // get threshold
    double thresh = (double) A5;
    // sample a line and get all points that have high variance
    vector<Scalar> points = cv_line_seg_pts_var_geq_T(seq, st, ed, thresh, rad);
    return A6 = point_vec2list(points);
}

/* line_pts_scharr_geq_T(IMGSEQ, [PX, PY, PZ], [A, B, C], T_SCHARR, P_LIST)
 *     equation of the line to be sampled:
 *         (X-PX)/A=(Y-PY)/B=(Z-PZ)/C
 * @(PX, PY, PZ) is a point that it crosses
 * @(A, B, C) is the direction of the line
 * @T_SCHARR is the threshold of scharr gradient
 * @P_LIST is the returned points that exceed the variance threshold on the
 *    line
 */
PREDICATE(line_pts_scharr_geq_T, 5) {
    char *p1 = (char*) A1;
    // coordinates scalar
    vector<int> pt_vec = list2vec<int>(A2, 3);
    Scalar pt(pt_vec[0], pt_vec[1], pt_vec[2]);
    // direction scalar
    vector<double> dr_vec = list2vec<double>(A3, 3);
    Scalar dir(dr_vec[0], dr_vec[1], dr_vec[2]);
    // get image sequence and compute variance
    const string add_seq(p1);
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    // get threshold
    double thresh = (double) A4;

    // sample a line and get all points that have high variance
    vector<Scalar> points = cv_line_pts_scharr_geq_T(seq, pt, dir, thresh);
    return A5 = point_vec2list(points);
}

/* line_pts_scharr_geq_T_2d(IMG, [PX, PY], [A, B], T_SCHARR, P_LIST)
 *     equation of the line to be sampled:
 *         (X-PX)/A=(Y-PY)/B=(Z-PZ)/C
 * @(PX, PY) is a point that it crosses
 * @(A, B) is the direction of the line
 * @T_SCHARR is the threshold of scharr gradient
 * @P_LIST is the returned points that exceed the variance threshold on the
 *    line
 */
PREDICATE(line_pts_scharr_geq_T_2d, 5) {
    char *p1 = (char*) A1;
    // coordinates scalar
    vector<int> pt_vec = list2vec<int>(A2, 2);
    Scalar pt(pt_vec[0], pt_vec[1], 0.0);
    // direction scalar
    vector<double> dr_vec = list2vec<double>(A3, 2);
    Scalar dir(dr_vec[0], dr_vec[1], 0.0);
    // get image sequence and compute variance
    const string add_img(p1);
    Mat *img = str2ptr<Mat>(add_img);
    // get threshold
    double thresh = (double) A4;

    // sample a line and get all points that have high variance
    vector<Scalar> points = cv_line_pts_scharr_geq_T_2d(img, pt, dir, thresh);
    return A5 = point_vec2list(points, -1, 2);
}

/* line_seg_pts_scharr_geq_T(IMGSEQ, [SX, SY, SZ], [EX, EY, EZ], T_SCHARR, P_LIST)
 *     equation of the line to be sampled:
 *         (X-PX)/A=(Y-PY)/B=(Z-PZ)/C
 * @[SX, SY, SZ] is starting point of the line segment
 * @[EX, EY, EZ] is ending point of the line segment
 * @T_SCHARR is the threshold of Scharr gradients
 * @P_LIST is the returned points that exceed the variance threshold on the
 *    line
 */
PREDICATE(line_seg_pts_scharr_geq_T, 5) {
    char *p1 = (char*) A1;
    // start point scalar
    vector<int> st_vec = list2vec<int>(A2, 3);
    Scalar st(st_vec[0], st_vec[1], st_vec[2]);
    // end point scalar
    vector<int> ed_vec = list2vec<int>(A3, 3);
    Scalar ed(ed_vec[0], ed_vec[1], ed_vec[2]);
    // get image sequence and compute variance
    const string add_seq(p1);
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    // get threshold
    double thresh = (double) A4;
    
    // sample a line and get all points that have high variance
    vector<Scalar> points = cv_line_seg_pts_scharr_geq_T(seq, st, ed, thresh);
    return A5 = point_vec2list(points);
}


/* fit_elps(PTS, CENTRE, PARAM)
 * given a list (>=5) of points, fit an ellipse on a plane (the 3rd dimenstion
 * is fixed)
 * @PTS: points list 
 * @CENTRE = [X, Y, _]: centre point of the ellipse
 * @PARAM = [A, B, ALPHA]: axis length (A >= B) and tilt angle (ALPHA)
 *      of the ellipse.
 * !!The unit of angle is DEG, not RAD; smaller than 1 then random angle!!
 */
PREDICATE(fit_elps, 3) {
    vector<Scalar> pts = point_list2vec(A1);
    if (pts.size() < 5) {
        cout << "[ERROR] At least 5 points are needed for fitting an ellipse."
             << endl;
        return FALSE;
    }
    // check if all points are on the same frame
    int frame = pts[0][2];
    for (auto it = pts.begin(); it != pts.end(); ++it) {
        Scalar pt = *it;
        if (frame != pt[2]) {
            cout << "[ERROR] Points are not on the same frame!" << endl;
            return FALSE;
        }
    }
    // fit ellipse
    Scalar cen;
    Scalar param;
    //opencv_fit_ellipse(pts, cen, param);
    if (!fit_ellipse_2d(pts, cen, param))
        return FALSE;
    
    for (int i = 0; i < 3; i++)
        if (isnan(param[i]))
            return FALSE;
    
    for (int i = 0; i < 3; i++)
        if (isnan(cen[i]))
            return FALSE;

    // bind variables
    vector<long> cen_vec = {(long) cen[0],
                            (long) cen[1],
                            (long) cen[2]};
    vector<long> param_vec = {(long) param[0],
                              (long) param[1],
                              (long) param[2]};
    A2 = vec2list<long>(cen_vec);
    A3 = vec2list<long>(param_vec);
    return TRUE;
}

/* fit_circle(+PTS, -CENTER, -RADIUS)
 * given a list (>=3) of points, fit a circle on a plane (the 3rd dimenstion
 * is fixed)
 * @PTS: points list 
 * @CENTER = [X, Y, F]: center position (X, Y, Frame)
 * @RADIUS: radius
 */
PREDICATE(fit_circle, 3) {
    vector<Scalar> pts = point_list2vec(A1);
    if (pts.size() < 3) {
        cout << "[ERROR] At least 3 points are needed for fitting a circle." << endl;
        return FALSE;
    }
    // check if all points are on the same frame
    int frame = pts[0][2];
    for (auto it = pts.begin(); it != pts.end(); ++it) {
        Scalar pt = *it;
        if (frame != pt[2]) {
            cout << "[ERROR] Points are not on the same frame!" << endl;
            return FALSE;
        }
    }
    // fit circle
    Scalar param;
    fit_circle_2d(pts, param);

    for (int i = 0; i < 3; i++)
        if (isnan(param[i]))
            return FALSE;
    
    // bind variables
    vector<long> center_vec = {(long) param[0],
                               (long) param[1],
                               frame};
    A2 = vec2list<long>(center_vec);
    A3 = PlTerm((long) param[2]);
    return TRUE;
}

/* points_color_hist(+IMGSEQ, +PTS, -HIST)
 * compute the color histogram vector of given point list
 * @IMG_SEQ: image sequence
 * @PTS: points [[x, y, frame] |...]
 * @HIST: [V1_1, V1_2, V1_3, V2_1, ..., V32_3] as
 *        [L1,   A1,   B1,   L2,   ..., B32]
 */
PREDICATE(points_color_hist, 3) {
    // image sequence
    char *p1 = (char*) A1;
    const string add_seq(p1);
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);
    // point lists
    vector<Scalar> pts = point_list2vec(A2);
    // get histogram
    vector<double> hist = cv_points_color_hist(seq, pts);
    return A3 = vec2list<double>(hist);
}
/* hist_diff(+HIST_1, +HIST_2, -DIFF)
 * compute the KL divergence between 2 color histograms
 * @HIST_1/2: input histograms, each one is a list
 *            [V1_1, V1_2, V1_3, V2_1, ..., V32_3] as
 *            [L1,   A1,   B1,   L2,   ..., B32]
 * @DIFF: output
 */

PREDICATE(hist_diff, 3) {
    vector<double> h1 = list2vec<double>(A1);
    vector<double> h2 = list2vec<double>(A2);
    int n = h1.size();
    // calculate KL divergence
    Scalar kls(0.0, 0.0, 0.0);
    double D_1_2_L = .0;
    double D_2_1_L = .0;
    double D_1_2_A = .0;
    double D_2_1_A = .0;
    double D_1_2_B = .0;
    double D_2_1_B = .0;

    for (int i = 0; i < n; i++) {
        if (i % 3 == 0) { // L
            D_1_2_L += h1[i]*log2(h1[i]/h2[i]);
            D_2_1_L += h2[i]*log2(h2[i]/h1[i]);
        }
        else if (i % 3 == 1) {// A
            D_1_2_A += h1[i]*log2(h1[i]/h2[i]);
            D_2_1_A += h2[i]*log2(h2[i]/h1[i]);
        }
        else {// B
            D_1_2_B += h1[i]*log2(h1[i]/h2[i]);
            D_2_1_B += h2[i]*log2(h2[i]/h1[i]);
        }
    }
    kls[0] = (D_1_2_L + D_2_1_L)/2;
    kls[1] = (D_1_2_A + D_2_1_A)/2;
    kls[2] = (D_1_2_B + D_2_1_B)/2;
    // quadratic mean of 3 channels
    double kl = sqrt((kls[0]*kls[0] + kls[1]*kls[1] + kls[2]*kls[2])/3);
    return A3 = kl;
}

/* color_hist_rect_2d(+Img, +Center, +Radius, -HistVector)
 * Img: pointer to image
 * Center: [X, Y], the center of the rectangle to be sampled
 * Radius: [RX, RY], the radius of the rectangle to be sampled
 * HistVector: histogram vector, a 96-dim vector
 */
PREDICATE(color_hist_rect_2d, 4) {
    // image
    char *p1 = (char*) A1;
    const string add_img(p1);
    cv::Mat *img = str2ptr<cv::Mat>(add_img);
    // parameters
    vector<int> cen_vec = list2vec<int>(A2, 2);
    Scalar cen(cen_vec[0], cen_vec[1]);
    vector<int> rad_vec = list2vec<int>(A3, 2);
    Scalar rad(rad_vec[0], rad_vec[1]);
    //
    vector<double> hist = cv_rect_masked_color_hist_2d(img, cen, rad);
    return A4 = vec2list<double>(hist);
}

/* color_L_hist_rect_2d(+Img, +Center, +Radius, -HistVector)
 * ONLY CALCULATE HISTOGRAM OF BRIGHTNESS
 * Img: pointer to image
 * Center: [X, Y], the center of the rectangle to be sampled
 * Radius: [RX, RY], the radius of the rectangle to be sampled
 * HistVector: histogram vector, a 96-dim vector
 */
PREDICATE(color_L_hist_rect_2d, 4) {
    // image
    char *p1 = (char*) A1;
    const string add_img(p1);
    cv::Mat *img = str2ptr<cv::Mat>(add_img);
    // parameters
    vector<int> cen_vec = list2vec<int>(A2, 2);
    Scalar cen(cen_vec[0], cen_vec[1]);
    vector<int> rad_vec = list2vec<int>(A3, 2);
    Scalar rad(rad_vec[0], rad_vec[1]);
    //
    vector<double> hist = cv_rect_masked_L_hist_2d(img, cen, rad);
    return A4 = vec2list<double>(hist);
}
