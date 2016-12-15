/* Sampling module
 * ================================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

#ifndef _SAMPLER_HPP
#define _SAMPLER_HPP

#include "utils.hpp"

#include <opencv2/core/core.hpp>
#include <opencv2/imgproc.hpp>
#include <opencv2/highgui/highgui.hpp>  // OpenCV window I/O
#include <armadillo>
#include <SWI-cpp.h>
#include <SWI-Prolog.h>

#include <iostream> // for standard I/O
#include <cstdlib>
#include <algorithm>
#include <string>   // for strings
#include <cmath>

using namespace std;
using namespace cv;

#define SWAP(a, b) {a = a + b; b = a - b; a = a - b;}

enum { XY_SHIFT = 16, XY_ONE = 1 << XY_SHIFT, DRAWING_STORAGE_BLOCK = (1 << 12) - 256 };

/********* declarations *********/
/* compute the variance of a set of points in image sequence */
double cv_imgs_points_var(vector<Mat> *images, vector<Scalar> point_set);

/* bound an local area in 3-d space and return the left/right up/down most
 *     points of the local area
 * @point: center
 * @radius: radius of the local area
 * @bound: boundary of the global space (> 0)
 * @return: left/right up/down most point of this area (grid)
 */
vector<Scalar> bound_scalar_3d(Scalar point, Scalar radius, Scalar bound);

/* get LAB color of a point (or average color in its neighborhood)
 * @images: image sequence
 * @point: position of the interest point
 * @radius: radius of the ellipsoid of the local area
 * @return: color in current location
 * REMARK: the 3 dimensions are width, height, duration
 */
Scalar cv_imgs_point_color_loc(vector<Mat> *images, Scalar point,
                               Scalar radius = Scalar(0, 0, 0));
Scalar cv_img_point_color_loc_2d(Mat *image, Scalar point,
                                 Scalar radius = Scalar(0, 0, 0));

/* calculate image local variance
 * @images: image sequence
 * @point: position of the interest point
 * @radius: radius of the ellipsoid of the local area
 * @return: variation of all the points
 * REMARK: the 3 dimensions are width, height, duration
 */
double cv_imgs_point_var_loc(vector<Mat> *images, Scalar point,
                             Scalar radius = Scalar(3, 3, 0));

/* calculate image gradient with Scharr operator
 * @image(s): image (sequence)
 * @point: position of the interest point
 * @return: variation of all the points 
 */
double cv_img_point_scharr(Mat *img, Scalar point);
double cv_imgs_point_scharr(vector<Mat> *images, Scalar point);
vector<double> cv_img_points_scharr(Mat *image,
                                    vector<Scalar> points);
vector<double> cv_imgs_points_scharr(vector<Mat> *images,
                                     vector<Scalar> points);

/* calculate image local color of a set of points
 * @images: image sequence
 * @points: position of the interest points
 * @radius: radius of the ellipsoid of the local area
 * @return: variation of all the points
 * REMARK: the 3 dimensions are width, height, duration
 */
vector<Scalar> cv_imgs_points_color_loc(vector<Mat> *images,
                                        vector<Scalar> points,
                                        Scalar radius = Scalar(0, 0, 0));
vector<Scalar> cv_img_points_color_loc_2d(Mat *image,
                                          vector<Scalar> points,
                                          Scalar radius = Scalar(0, 0, 0));

/* calculate image local variance of a set of points
 * @images: image sequence
 * @points: position of the interest points
 * @radius: radius of the ellipsoid of the local area
 * @return: variation of all the points
 * REMARK: the 3 dimensions are width, height, duration
 */
vector<double> cv_imgs_points_var_loc(vector<Mat> *images,
                                      vector<Scalar> points,
                                      Scalar radius = Scalar(3, 3, 0));
/* get all points on a line
 * @point: position of a point on the line
 * @direction: direction of the line
 * @bound: size of the 3d-space
 * @return: all points on the line
 */
vector<Scalar> get_line_points(Scalar point, Scalar direction, Scalar bound);

/* get all points on a line segment
 * @start: position of a point on the line
 * @end: direction of the line
 * @bound: size of the 3d-space
 * @return: all points on the line segment
 */
vector<Scalar> get_line_seg_points(Scalar start, Scalar end, Scalar bound);

/* get all points on an ellipse/circle (ONLY FOR 2D IMAGE,
 *    SO z ALWAYS EQUALS TO 0) When sampling a specific frame, remember
 *    to change z value of all points
  * @centre: centre point of the circle
 * @param: parameters of an ellipse (long/short axis length and
 *     axis angle (DEG, not RAD!!!))
 * @bound: size of the 3d-space
 * @return: all points on the circle
 */
vector<Scalar> get_ellipse_points(Scalar centre, Scalar param, Scalar bound);
vector<Scalar> get_circle_points(Scalar param, Scalar bound);

/* get all points in an cube
 *    When sampling a specific frame, remember to change z value of all points
 * @centre: centre point of the cube
 * @param: parameters of an cube (length in x, y, z directions)
 * @bound: size of the 3d-space
 * @return: all points in the cube
 */
vector<Scalar> get_in_cube_points(Scalar centre, Scalar param, Scalar bound);


/* sample a line in 3d space and return the points whose local variance 
 * exceeds the given threshold
 * @images: image sequence
 * @point: position of a point
 * @direction: direction of the line
 * @var_threshold: variance threshold
 * @loc_radius: local area size
 * @return: points meet the requirement
 */
vector<Scalar> cv_line_pts_var_geq_T(vector<Mat> *images, Scalar point,
                              Scalar direction, double var_threshold = 2.0,
                              Scalar loc_radius = Scalar(5, 5, 0));

/* sample a line in 3d space and return the points whose local variance 
 * exceeds the given threshold
 * @images: image sequence
 * @start: start point of line segment
 * @end: end point of line segment
 * @var_threshold: variance threshold
 * @loc_radius: local area size
 * @return: points meet the requirement
 */
vector<Scalar> cv_line_seg_pts_var_geq_T(vector<Mat> *images, Scalar start,
                              Scalar end, double var_threshold = 2.0,
                              Scalar loc_radius = Scalar(5, 5, 0));

/* sample a line in 3d space and return the points whose scharr gradient
 * exceeds the given threshold WITH THINNING
 * @images: image sequence
 * @point: position of a point
 * @direction: direction of the line
 * @grad_threshold: variance threshold
 * @return: points meet the requirement
 */
vector<Scalar> cv_line_pts_scharr_geq_T(vector<Mat> *images,
                                        Scalar point,
                                        Scalar direction,
                                        double var_threshold = 5.0);
vector<Scalar> cv_line_pts_scharr_geq_T_2d(Mat *image,
                                           Scalar point,
                                           Scalar direction,
                                           double var_threshold = 5.0);
vector<Scalar> cv_line_seg_pts_scharr_geq_T(vector<Mat> *images,
                                            Scalar start,
                                            Scalar end,
                                            double var_threshold = 5.0);

/* bresenham's line algorithm
 * @current: starting point to be extended
 * @direction: direction of line extention
 * @inc: increment direction
 * @bound: 3-d space size
 * @&points: vector line points
 * @front: whether new points are pushed to vector's front
 */
void bresenham(Scalar current, Scalar direction, Scalar inc,
               Scalar bound, vector<Scalar> *points);

/* fits a set of points with an ellipse on an 2d image
 * Based on:
 *   Fitzgibbon, A.W., Pilu, M., and Fischer R.B., Direct least squares
 *   fitting of ellipsees, Proc. of the 13th Internation Conference on Pattern
 *   Recognition, pp 253–257, Vienna, 1996.
 * @points: points for fitting
 * @centre: center of the ellipse
 * @param: other parameters (long/short axis length and axis angle)
 */
void opencv_fit_ellipse(vector<Scalar> points, Scalar& centre, Scalar& param);

/* fits a set of points with a circle on an 2d image
 * @points: points for fitting
 * @param: parameter of the circle (centerX, centerY, radius)
 */
void fit_circle_2d(vector<Scalar> points, Scalar& param);

/* compute the color histogram of a set of points in image sequence
 * @images: image sequence
 * @points: point set
 */
vector<double> cv_points_color_hist(vector<Mat> *images,
                                    vector<Scalar> points);

/* Compare two sets of points' histogram distributions, return KL divergence
 * @images: image sequence
 * @points_1: point set 1
 * @points_2: point set 2
 */
double compare_hist(vector<Mat> *images,
                    vector<Scalar> points_1,
                    vector<Scalar> points_2);

/* determines whether a 3D point is out of a 3D space (positive)
 * @point: input point
 * @bound: boundary of the 3D space (>= 0)
 */
bool out_of_canvas(Scalar point, Scalar bound) {
    int x = point[0];
    int y = point[1];
    int z = point[2];
    
    if (x >= 0 && x <= bound[0] - 1 &&
        y >= 0 && y <= bound[1] - 1 &&
        z >= 0 && z <= bound[2] - 1) 
        return false;
    else
        return true;
}
/* determines whether two points are continuous
 * @p1, p2: input points
 */
bool point_cont(Scalar p1, Scalar p2) {
    arma::vec v = {(double) abs(p1[0] - p2[0]),
                   (double) abs(p1[1] - p2[1]),
                   (double) abs(p1[2] - p2[2])};
    if (v.max() < 2)
        return true;
    else
        return false;
}


/********* implementations *********/
double cv_imgs_point_var_loc(vector<Mat> *images, Scalar point,
                             Scalar radius) {
    // get left_up_most and right_down_most and enumerate all pixels
    int w = (*images)[0].cols;
    int h = (*images)[0].rows;
    int d = images->size();
    vector<Scalar> bounds = bound_scalar_3d(point, radius, Scalar(w, h, d));
    Scalar left_up_most = bounds[0];
    Scalar right_down_most = bounds[1];

    // enumerate pixels that position in local ellipsoid
    // the ellipsoid is:
    //  ((X - P1)/W)^2 + ((Y - P2)/H)^2 + ((Z - P3)/D )^2 = 1
    
    // collect pixels
    vector<Scalar> point_set;
    for (auto i = left_up_most[2]; i <= right_down_most[2]; i++)
        for (auto r = left_up_most[1]; r <= right_down_most[1]; r++)
            for (auto c = left_up_most[0]; c <= right_down_most[0]; c++) {
                double p1, p2, p3;
                p1 = radius[0] > 0 ?
                    pow(((double) c - point[0])/radius[0], 2) : 0;
                p2 = radius[1] > 0 ?
                    pow(((double) r - point[1])/radius[1], 2) : 0;
                p3 = radius[2] > 0 ?
                    pow(((double) i - point[2])/radius[2], 2) : 0;
                /* debug
                cout << c << "-" << point[0] << ","
                     << r << "-" << point[1] << ","
                     << i << "-" << point[2] << "," << "\n\t";
                cout << p1 << ","
                     << p2 << ","
                     << p3 << endl;
                */
                if (p1 + p2 + p3 <= 1.0) {
                    point_set.push_back(Scalar(c, r, i));

                }
            }

    return cv_imgs_points_var(images, point_set);
}

double cv_imgs_points_var(vector<Mat> *images, vector<Scalar> point_set) {
    int count = point_set.size();
    Scalar avg = Scalar(.0, .0, .0);
    // compute average
    for (auto it = point_set.begin(); it != point_set.end(); ++it) {
        Scalar pos = *it; // position of the pixel
        Mat img = (*images)[pos[2]];
        Vec3b pixel = img.at<Vec3b>(pos[1], pos[0]);
        for (int channel = 0; channel < 3; channel++)
            avg[channel] += pixel[channel];

    }
    
    avg = avg/(double) count;
    //cout << "avg:\t" << avg[0] << ", " << avg[1] << ", " << avg[2] << endl;    
    // compute variance
    Scalar var(.0, .0, .0);
    for (auto it = point_set.begin(); it != point_set.end(); ++it) {
        Scalar pos = *it; // position of the pixel
        Mat img = (*images)[pos[2]];
        Vec3b pixel = img.at<Vec3b>(pos[1], pos[0]);
        for (int channel = 0; channel < 3; channel++) {
            double res = pixel[channel] - avg[channel];
            var[channel] += pow(res, 2);
        }
    }

    //var = var/(double) (count - 1);
    //cout << "var:\t" << var[0] << ", " << var[1] << ", " << var[2] << endl;
    double std = sqrt(var[0] / (double) (count - 1))
        + sqrt(var[1] / (double) (count - 1))
        + sqrt(var[2] / (double) (count - 1));
    return std;
}


double cv_imgs_point_scharr(vector<Mat> *images, Scalar point) {
    // get left_up_most and right_down_most and enumerate all pixels
    int w = (*images)[0].cols;
    int h = (*images)[0].rows;
    // point position
    int x = point[0];
    int y = point[1];
    int frame = point[2];
    if (x < 1 || y < 1 || x > w - 2 || y > h - 2)
        return 0.0;
    // build brightness matrix
    Mat img = (*images)[frame];
    arma::mat A = {{(double) img.at<Vec3b>(y-1, x-1)[0],
                    (double) img.at<Vec3b>(y-1, x)[0],
                    (double) img.at<Vec3b>(y-1, x+1)[0]},
                   {(double) img.at<Vec3b>(y, x-1)[0],
                    (double) img.at<Vec3b>(y, x)[0],
                    (double) img.at<Vec3b>(y, x+1)[0]},
                   {(double) img.at<Vec3b>(y+1, x-1)[0],
                    (double) img.at<Vec3b>(y+1, x)[0],
                    (double) img.at<Vec3b>(y+1, x+1)[0]}};
    // calculate Scharr gradients in brightness channel
    arma::mat gx = {{3, 10, 3},
                    {0, 0, 0},
                    {-3, -10, -3}};
    arma::mat gy = {{3, 0, -3},
                    {10, 0, -10},
                    {3, 0, -3}};
    gx = gx/32;
    gy = gy/32;
    double Gx = gx(2,2)*A(0,0) + gx(2,1)*A(0,1) + gx(2,0)*A(0,2)
        + gx(1,2)*A(1,0) + gx(1,1)*A(1,1) + gx(1,0)*A(1,2)
        + gx(0,2)*A(2,0) + gx(0,1)*A(2,1) + gx(0,0)*A(2,2);
    double Gy = gy(2,2)*A(0,0) + gy(2,1)*A(0,1) + gy(2,0)*A(0,2)
        + gy(1,2)*A(1,0) + gy(1,1)*A(1,1) + gy(1,0)*A(1,2)
        + gy(0,2)*A(2,0) + gy(0,1)*A(2,1) + gy(0,0)*A(2,2);
    double G = sqrt(Gx*Gx + Gy*Gy);
    return G;
}

double cv_img_point_scharr(Mat *img, Scalar point) {
    int w = img->cols;
    int h = img->rows;
    // point position
    int x = point[0];
    int y = point[1];
    if (x < 1 || y < 1 || x > w - 2 || y > h - 2)
        return 0.0;
    // build brightness matrix
    arma::mat A = {{(double) img->at<Vec3b>(y-1, x-1)[0],
                    (double) img->at<Vec3b>(y-1, x)[0],
                    (double) img->at<Vec3b>(y-1, x+1)[0]},
                   {(double) img->at<Vec3b>(y, x-1)[0],
                    (double) img->at<Vec3b>(y, x)[0],
                    (double) img->at<Vec3b>(y, x+1)[0]},
                   {(double) img->at<Vec3b>(y+1, x-1)[0],
                    (double) img->at<Vec3b>(y+1, x)[0],
                    (double) img->at<Vec3b>(y+1, x+1)[0]}};
    // calculate Scharr gradients in brightness channel
    arma::mat gx = {{3, 10, 3},
                    {0, 0, 0},
                    {-3, -10, -3}};
    arma::mat gy = {{3, 0, -3},
                    {10, 0, -10},
                    {3, 0, -3}};
    gx = gx/32;
    gy = gy/32;
    double Gx = gx(2,2)*A(0,0) + gx(2,1)*A(0,1) + gx(2,0)*A(0,2)
        + gx(1,2)*A(1,0) + gx(1,1)*A(1,1) + gx(1,0)*A(1,2)
        + gx(0,2)*A(2,0) + gx(0,1)*A(2,1) + gx(0,0)*A(2,2);
    double Gy = gy(2,2)*A(0,0) + gy(2,1)*A(0,1) + gy(2,0)*A(0,2)
        + gy(1,2)*A(1,0) + gy(1,1)*A(1,1) + gy(1,0)*A(1,2)
        + gy(0,2)*A(2,0) + gy(0,1)*A(2,1) + gy(0,0)*A(2,2);
    double G = sqrt(Gx*Gx + Gy*Gy);
    return G;
}

Scalar cv_imgs_point_color_loc(vector<Mat> *images, Scalar point,
                               Scalar radius) {
    // canvas size
    int w = (*images)[0].cols;
    int h = (*images)[0].rows;
    int d = images->size();
    // bounded neighborhood
    vector<Scalar> bounds = bound_scalar_3d(point, radius, Scalar(w, h, d));
    Scalar left_up_most = bounds[0];
    Scalar right_down_most = bounds[1];
    
    // collect pixels 
    vector<Scalar> point_set;
    for (auto i = left_up_most[2]; i <= right_down_most[2]; i++)
        for (auto r = left_up_most[1]; r <= right_down_most[1]; r++)
            for (auto c = left_up_most[0]; c <= right_down_most[0]; c++) {
                double p1, p2, p3;
                p1 = radius[0] > 0 ?
                    pow(((double) c - point[0])/radius[0], 2) : 0;
                p2 = radius[1] > 0 ?
                    pow(((double) r - point[1])/radius[1], 2) : 0;
                p3 = radius[2] > 0 ?
                    pow(((double) i - point[2])/radius[2], 2) : 0;
                /* debug
                cout << c << "-" << point[0] << ","
                     << r << "-" << point[1] << ","
                     << i << "-" << point[2] << "," << "\n\t";
                cout << p1 << ","
                     << p2 << ","
                     << p3 << endl;
                */
                if (p1 + p2 + p3 <= 1.0) {
                    point_set.push_back(Scalar(c, r, i));
                }
            }

    int count = point_set.size();
    Scalar avg = Scalar(.0, .0, .0);
    // compute average
    for (auto it = point_set.begin(); it != point_set.end(); ++it) {
        Scalar pos = *it; // position of the pixel
        Mat img = (*images)[pos[2]];
        Vec3b pixel = img.at<Vec3b>(pos[1], pos[0]);
        for (int channel = 0; channel < 3; channel++)
            avg[channel] += pixel[channel];

    }
    
    avg = avg/(double) count;
    return avg;
}

Scalar cv_img_point_color_loc_2d(Mat *image, Scalar point,
                                 Scalar radius) {
    // canvas size
    int w = image->cols;
    int h = image->rows;
    // bounded neighborhood
    vector<Scalar> bounds = bound_scalar_3d(point, radius, Scalar(w, h, 1));
    Scalar left_up_most = bounds[0];
    Scalar right_down_most = bounds[1];
    
    // collect pixels 
    vector<Scalar> point_set;
    for (auto i = left_up_most[2]; i <= right_down_most[2]; i++)
        for (auto r = left_up_most[1]; r <= right_down_most[1]; r++)
            for (auto c = left_up_most[0]; c <= right_down_most[0]; c++) {
                double p1, p2, p3;
                p1 = radius[0] > 0 ?
                    pow(((double) c - point[0])/radius[0], 2) : 0;
                p2 = radius[1] > 0 ?
                    pow(((double) r - point[1])/radius[1], 2) : 0;
                p3 = radius[2] > 0 ?
                    pow(((double) i - point[2])/radius[2], 2) : 0;
                if (p1 + p2 + p3 <= 1.0) {
                    point_set.push_back(Scalar(c, r, i));
                }
            }

    int count = point_set.size();
    Scalar avg = Scalar(.0, .0, .0);
    // compute average
    for (auto it = point_set.begin(); it != point_set.end(); ++it) {
        Scalar pos = *it; // position of the pixel
        Vec3b pixel = image->at<Vec3b>(pos[1], pos[0]);
        for (int channel = 0; channel < 3; channel++)
            avg[channel] += pixel[channel];

    }
    
    avg = avg/(double) count;
    return avg;
}

vector<Scalar> cv_imgs_points_color_loc(vector<Mat> *images,
                                        vector<Scalar> points,
                                        Scalar radius) {
    vector<Scalar> re;
    for (auto it = points.begin(); it != points.end(); ++it)
        re.push_back(cv_imgs_point_color_loc(images, (Scalar) *it, radius));
    return re;
}

vector<Scalar> cv_img_points_color_loc_2d(Mat *image,
                                          vector<Scalar> points,
                                          Scalar radius) {
    vector<Scalar> re;
    for (auto it = points.begin(); it != points.end(); ++it) {
        Scalar pt = (Scalar) *it;
        pt[2] = 0;
        re.push_back(cv_img_point_color_loc_2d(image, pt, radius));
    }
    return re;
}

vector<double> cv_imgs_points_var_loc(vector<Mat> *images,
                                      vector<Scalar> points,
                                      Scalar radius) {
    vector<double> re;
    for (auto it = points.begin(); it != points.end(); ++it)
        re.push_back(cv_imgs_point_var_loc(images, (Scalar) *it, radius));
    return re;
}

vector<double> cv_imgs_points_scharr(vector<Mat> *images,
                                     vector<Scalar> points) {
    vector<double> re;
    for (auto it = points.begin(); it != points.end(); ++it)
        re.push_back(cv_imgs_point_scharr(images, (Scalar) *it));
    return re;
}

vector<double> cv_img_points_scharr(Mat *image,
                                    vector<Scalar> points) {
    vector<double> re;
    for (auto it = points.begin(); it != points.end(); ++it)
        re.push_back(cv_img_point_scharr(image, (Scalar) *it));
    return re;
}

vector<Scalar> bound_scalar_3d(Scalar point, Scalar radius, Scalar bound) {
    Scalar left_up_most(10000, 10000, 10000);
    Scalar right_down_most(-10000, -10000, -10000);
    // the order of Mat is "column, row, duration"
    for (auto i = 0; i < 3; i++) {
        left_up_most[i] = point[i] - radius[i] < 0 ? 0 : point[i] - radius[i];
        right_down_most[i] = point[i] + radius[i] >= bound[i] ? bound[i] - 1 : point[i] + radius[i];
    }
    vector<Scalar> re;
    re.push_back(left_up_most);
    re.push_back(right_down_most);
    return re;
}

vector<Scalar> cv_line_pts_var_geq_T(vector<Mat> *images, Scalar point,
                              Scalar direction, double var_threshold,
                              Scalar loc_radius){
    vector<Scalar> re;
    // size of the 3-d space
    Scalar bound((*images)[0].cols, (*images)[0].rows, images->size());
    // get all points on this line
    vector<Scalar> line_points = get_line_points(point, direction, bound);
    // evaluate local variance of all points
    for (auto it = line_points.begin(); it != line_points.end(); ++it) {
        Scalar pt = *it;
        double var =  cv_imgs_point_var_loc(images, pt, loc_radius);
        // debug
        cout << (*it)[0] << ","
             << (*it)[1] << ","
             << (*it)[2] << "\t";
        cout << var << endl;
        //
        if (var >= var_threshold)
            re.push_back(pt);
    }
    return re;
}

vector<Scalar> cv_line_seg_pts_var_geq_T(vector<Mat> *images, Scalar start,
                              Scalar end, double var_threshold,
                              Scalar loc_radius){
    vector<Scalar> re;
    // size of the 3-d space
    Scalar bound((*images)[0].cols, (*images)[0].rows, images->size());
    // get all points on this line
    vector<Scalar> line_points = get_line_seg_points(start, end, bound);
    // evaluate local variance of all points
    for (auto it = line_points.begin(); it != line_points.end(); ++it) {
        Scalar pt = *it;
        double var =  cv_imgs_point_var_loc(images, pt, loc_radius);
        /* debug
        cout << (*it)[0] << ","
             << (*it)[1] << ","
             << (*it)[2] << "\t";
        cout << var << endl;
        */
        if (var >= var_threshold)
            re.push_back(pt);
    }
    return re;
}

// 3D Bresenham's line generation
vector<Scalar> get_line_points(Scalar point, Scalar direction,
                               Scalar bound) {
    // REMARK: all coordinate oders as "column, row, duration"
    vector<Scalar> re; // returned point list

    if (direction[0] == 0 &&
        direction[1] == 0 &&
        direction[2] == 0)
        return re;
    
    // increment in each direction
    int c_inc = direction[0] > 0 ? 1 : -1;
    int r_inc = direction[1] > 0 ? 1 : -1;
    int d_inc = direction[2] > 0 ? 1 : -1;
    Scalar inc;
    inc[0] = c_inc;
    inc[1] = r_inc;
    inc[2] = d_inc;
    bresenham(point, direction, inc, bound, &re);

    reverse(re.begin(), re.end());
    re.push_back(point); // insert itself
    // grow in another direction
    c_inc = -1 * c_inc;
    r_inc = -1 * r_inc;
    d_inc = -1 * d_inc;
    inc[0] = c_inc;
    inc[1] = r_inc;
    inc[2] = d_inc;
    bresenham(point, direction, inc, bound, &re);
    reverse(re.begin(), re.end());

    return re;
}

vector<Scalar> get_line_seg_points(Scalar start, Scalar end, Scalar bound) {
    vector<Scalar> re; // returned point list
    
    double x = start[0];
    double y = start[1];
    double z = start[2];
    double x2 = end[0];
    double y2 = end[1];
    double z2 = end[2];

    if (x == x2 &&
        y == y2 &&
        z == z2) {
        re.push_back(Scalar(x, y, z));
        return re;
    }
    // boundary check
    if (out_of_canvas(start, bound) && out_of_canvas(end, bound))
        return re;
    if (out_of_canvas(start, bound) && !out_of_canvas(end, bound)) {
        // if start point is out of canvas and end point is in the canvas,
        // swap the start and the end point
        SWAP(x, x2);
        SWAP(y, y2);
        SWAP(z, z2);
    }
    
    re.push_back(Scalar(x, y, z));
    
    // bresenham for line segment
    double mdx = abs(x2 - x);
    double mdy = abs(y2 - y);
    double mdz = abs(z2 - z);
    double ddx = x2 - x;
    double ddy = y2 - y;
    double ddz = z2 - z;

    // robustness for double input
    double min = mdx;
    if (min == 0 && mdy != 0)
        min = mdy;
    if (min == 0 && mdz != 0)
        min = mdz;
    if (mdy != 0 && min > mdy)
        SWAP(min, mdy);
    if (mdz != 0 && min > mdz)
        SWAP(min, mdz);
    int dx = round(ddx*1000/min);
    int dy = round(ddy*1000/min);
    int dz = round(ddz*1000/min);

    int x_inc = dx > 0 ? 1 : -1;
    int y_inc = dy > 0 ? 1 : -1;
    int z_inc = dz > 0 ? 1 : -1;

    int Adx = abs(dx);
    int Ady = abs(dy);
    int Adz = abs(dz);
    
    int dx2 = Adx*2;
    int dy2 = Ady*2;
    int dz2 = Adz*2;

    int err_1, err_2;

    while (!out_of_canvas(Scalar(x, y, z), bound) &&
           !(x == x2 && y == y2 && z == z2)) {
        if (Adx >= Ady && Adx >= Adz) {
            err_1 = dy2 - Adx;
            err_2 = dz2 - Adx;
            for (int cont = 0; cont < Adx; cont++) {
                if (err_1 > 0) { 
                    y += y_inc;
                    err_1 -= dx2;
                }
                if (err_2 > 0) {
                    z += z_inc;
                    err_2 -= dx2;
                }
                err_1 += dy2;
                err_2 += dz2;
                x += x_inc;
                Scalar point(x, y, z);
                if (!out_of_canvas(point, bound) &&
                    !(x == x2 && y == y2 && z == z2))
                    re.push_back(point);
                else
                    break;
            }
        }
        
        if (Ady > Adx && Ady >= Adz) {
            err_1 = dx2 - Ady;
            err_2 = dz2 - Ady;
            for (int cont = 0; cont < Ady; cont++) {
                if (err_1 > 0) {
                    x += x_inc;
                    err_1 -= dy2;
                }
                if (err_2 > 0) { 
                    z += z_inc;
                    err_2 -= dy2;
                }
                err_1 += dx2;
                err_2 += dz2;
                y += y_inc;
                Scalar point(x, y, z);
                if (!out_of_canvas(point, bound) &&
                    !(x == x2 && y == y2 && z == z2))
                    re.push_back(point);
                else
                    break;
            }
        }
        
        if (Adz > Adx && Adz > Ady) {
            err_1 = dy2 - Adz;
            err_2 = dx2 - Adz;
            for (int cont = 0; cont < Adz; cont++) {
                if (err_1 > 0) {
                    y += y_inc;
                    err_1 -= dz2;
                }
                if (err_2 > 0) {
                    x += x_inc;
                    err_2 -= dz2;
                }
                err_1 += dy2;
                err_2 += dx2;
                z += z_inc;
                Scalar point(x, y, z);
                if (!out_of_canvas(point, bound) &&
                    !(x == x2 && y == y2 && z == z2))
                    re.push_back(point);
                else
                    break;
            }
        }
    }
    re.push_back(Scalar(x2, y2, z2));
    return re;
}
/* bresenham for line, NOT segment */
void bresenham(Scalar current, Scalar direction, Scalar inc,
               Scalar bound, vector<Scalar> *points) {
    // magnitude
    double ddx = abs(direction[0]);
    double ddy = abs(direction[1]);
    double ddz = abs(direction[2]);

    // robustness for double input
    double min = ddx;
    if (min == 0 && ddy != 0)
        min = ddy;
    if (min == 0 && ddz != 0)
        min = ddz;
    if (ddy != 0 && min > ddy)
        SWAP(min, ddy);
    if (ddz != 0 && min > ddz)
        SWAP(min, ddz);
    
    // direction
    int dx = round(direction[0]*1000/min);
    int dy = round(direction[1]*1000/min);
    int dz = round(direction[2]*1000/min);
    
    int x_inc = inc[0];
    int y_inc = inc[1];
    int z_inc = inc[2];
    
    int Adx = abs(dx);
    int Ady = abs(dy);
    int Adz = abs(dz);
    
    int dx2 = Adx*2;
    int dy2 = Ady*2;
    int dz2 = Adz*2;
    
    int x = current[0];
    int y = current[1];
    int z = current[2];

    int err_1, err_2;
    
    if (Adx >= Ady && Adx >= Adz) { 
        err_1 = dy2 - Adx;
        err_2 = dz2 - Adx;
        for (int cont = 0; cont < Adx; cont++) {
            if (err_1 > 0) { 
                y += y_inc;
                err_1 -= dx2;
            }
            if (err_2 > 0) {
                z += z_inc;
                err_2 -= dx2;
            }
            err_1 += dy2;
            err_2 += dz2;
            x += x_inc;
            Scalar point(x, y, z);
            if (!out_of_canvas(point, bound))
                points->push_back(point);
            else
                break;
        }
    }
    
    if (Ady > Adx && Ady >= Adz) {
        err_1 = dx2 - Ady;
        err_2 = dz2 - Ady;
        for (int cont = 0; cont < Ady; cont++) {
            if (err_1 > 0) {
                x += x_inc;
                err_1 -= dy2;
            }
            if (err_2 > 0) { 
                z += z_inc;
                err_2 -= dy2;
            }
            err_1 += dx2;
            err_2 += dz2;
            y += y_inc;
            Scalar point(x, y, z);
            if (!out_of_canvas(point, bound))
                points->push_back(point);
            else
                break;
            
        }
    }
   
    if (Adz > Adx && Adz > Ady) {
        err_1 = dy2 - Adz;
        err_2 = dx2 - Adz;
        for (int cont = 0; cont < Adz; cont++) {
            if (err_1 > 0) {
                y += y_inc;
                err_1 -= dz2;
            }
            if (err_2 > 0) {
                x += x_inc;
                err_2 -= dz2;
            }
            err_1 += dy2;
            err_2 += dx2;
            z += z_inc;
            Scalar point(x, y, z);
            if (!out_of_canvas(point, bound))
                points->push_back(point);
            else
                break;
        }
    }
    
    Scalar point(x, y, z);
    if (!out_of_canvas(point, bound))
        bresenham(point, direction, inc, bound, points);
}

void opencv_fit_ellipse(vector<Scalar> points, Scalar& centre, Scalar& param) {
    std::vector<cv::Point2f> pointsf;
    for (size_t i = 0; i < points.size(); i++)
        pointsf.push_back(cv::Point2f(points[i][0], points[i][1]));
    cv::RotatedRect box = cv::fitEllipse(pointsf);
    int frame = points[0][2];
    // centre of ellipse
    centre = Scalar(round(box.center.x),
                    round(box.center.y), frame);
    // width, height, and rotate angle of ellipse
    //cout << "width and height: "
    //     << box.size.width << ", " << box.size.height << endl;
    param = Scalar(round(box.size.width/2), // we need radius = half of them
                   round(box.size.height/2),
                   box.angle);
}

void fit_circle_2d(vector<Scalar> points, Scalar& param) {
    int iter, IterMAX = 99;
    double Xi, Yi, Zi;
    double Mz, Mxy, Mxx, Myy, Mxz, Myz, Mzz, Cov_xy, Var_z;
    double a0, a1, a2, a22, a3, a33;
    double Dy, xnew, x, ynew, y;
    double DET, Xcenter, Ycenter;

    double meanX, meanY;
    meanX = meanY = 0.0;
    for (auto it = points.begin(); it != points.end(); ++it) {
        meanX += (*it)[0];
        meanY += (*it)[1];
    }
    meanX /= points.size();
    meanY /= points.size();

    // computing moments 
	Mxx = Myy = Mxy = Mxz = Myz = Mzz = 0.0;
    
    for (auto it = points.begin(); it != points.end(); ++it) {
        Xi = (*it)[0] - meanX;   //  centered x-coordinates
        Yi = (*it)[1] - meanY;   //  centered y-coordinates
        Zi = Xi*Xi + Yi*Yi;
        
        Mxy += Xi*Yi;
        Mxx += Xi*Xi;
        Myy += Yi*Yi;
        Mxz += Xi*Zi;
        Myz += Yi*Zi;
        Mzz += Zi*Zi;
    }
    Mxx /= points.size();
    Myy /= points.size();
    Mxy /= points.size();
    Mxz /= points.size();
    Myz /= points.size();
    Mzz /= points.size();
    
    // computing coefficients of the characteristic polynomial
    
    Mz = Mxx + Myy;
    Cov_xy = Mxx*Myy - Mxy*Mxy;
    Var_z = Mzz - Mz*Mz;
    a3 = 4.0*Mz;
    a2 = -3.0*Mz*Mz - Mzz;
    a1 = Var_z*Mz + 4.0*Cov_xy*Mz - Mxz*Mxz - Myz*Myz;
    a0 = Mxz*(Mxz*Myy - Myz*Mxy) + Myz*(Myz*Mxx - Mxz*Mxy) - Var_z*Cov_xy;
    a22 = a2 + a2;
    a33 = a3 + a3 + a3;

    // finding the root of the characteristic polynomial
    // using Newton's method starting at x = 0  
    // (it is guaranteed to converge to the right root)
    
    for (x = 0.,y = a0, iter = 0; iter < IterMAX; iter++) {
        Dy = a1 + x*(a22 + a33*x);
        xnew = x - y/Dy;
        if ((xnew == x) || (!isfinite(xnew))) break;
        ynew = a0 + xnew*(a1 + xnew*(a2 + xnew*a3));
        if (abs(ynew)>=abs(y))  break;
        x = xnew;  y = ynew;
    }
     
    // computing paramters of the fitting circle
    
    DET = x*x - x*Mz + Cov_xy;
    Xcenter = (Mxz*(Myy - x) - Myz*Mxy)/DET/2.0;
    Ycenter = (Myz*(Mxx - x) - Mxz*Mxy)/DET/2.0;

    // assembling the output

    param = Scalar(round(Xcenter + meanX), // X center
                   round(Ycenter + meanY), // Y center
                   sqrt(Xcenter*Xcenter + Ycenter*Ycenter + Mz)); // Z center
}

vector<Scalar> get_ellipse_points(Scalar centre, Scalar param, Scalar bound) {
    vector<Scalar> re;
    // detailness of using polylines to approximate ellipse
    int width = std::abs((int) param[0]);
    int height = std::abs((int) param[1]);
    //if (width > height)
    //    SWAP(width, height);

    std::vector<Point> v; // vertices
    //cout << "Starting OpenCV ellipse2Poly.\n";
    ellipse2Poly(Point(centre[0], centre[1]),
                 Size(width, height),
                 param[2], 0, 360, 3, v); // the "3" before "v" is a parameter to control the accuracy of the polygon-approximation to ellipse
    if((int) v.size() <= 0)
        return re;
    //cout << "OpenCV ellipse2Poly done.\n";
    //cout << "Totally " << v.size() << " vertices.\n";

    Point p0;
    p0 = v[0];
    //cout << "Starting polygon to line segment points.\n";
    for(int i = 1; i < (int) v.size(); i++) {
        //cout << "converting point #" << i;
        Point p = v[i];
        //cout << " (" << p.x << ", " << p.y << ")";
        // points on line segment
        vector<Scalar> temp_pts = get_line_seg_points(Scalar(p0.x, p0.y, centre[2]), Scalar(p.x, p.y, centre[2]), bound);
        re += temp_pts;
        //cout << ". Points num: " << temp_pts.size() << endl;;
        p0 = p;
    }
    //cout << "polygon to line segment points done.\n";
    return re;
}

vector<Scalar> get_circle_points(Scalar param, Scalar bound) {
    vector<Scalar> re;
    // detailness of using polylines to approximate ellipse
    int x = std::abs((int) param[0]);
    int y = std::abs((int) param[1]);
    int f = std::abs((int) param[2]);
    int r = std::abs((int) param[3]);

    std::vector<Point> v; // vertices
    ellipse2Poly(Point(x, y),
                 Size(r, r),
                 0, 0, 360, 3, v); // the "3" before "v" is a parameter to control the accuracy of the polygon-approximation to ellipse
    if((int) v.size() <= 0)
        return re;
    
    Point p0;
    p0 = v[0];
    for(int i = 1; i < (int) v.size(); i++) {
        Point p = v[i];
        // points on line segment
        vector<Scalar> temp_pts = get_line_seg_points(Scalar(p0.x, p0.y, f), Scalar(p.x, p.y, f), bound);
        re += temp_pts;
        p0 = p;
    }
    return re;
}

vector<Scalar> get_in_cube_points(Scalar centre, Scalar param, Scalar bound) {
    int x = (int) centre[0];
    int y = (int) centre[1];
    int z = (int) centre[2];
    
    int a = (int) param[0];
    int b = (int) param[1];
    int c = (int) param[2];

    vector<Scalar> re;

    for (int i = x - a; i <= x + a; i++)
        for (int j = y - b; j <= y + b; j++)
            for (int k = z - c; k <= z + c; k++) {
                Scalar point = Scalar(i, j, k);
                if (!out_of_canvas(point, bound))
                    re.push_back(point);
            }
    return re;
}

vector<Scalar> cv_line_pts_scharr_geq_T_2d(Mat *image,
                                           Scalar point,
                                           Scalar direction,
                                           double grad_threshold) {
    vector<Scalar> re;
    // size of the 3-d space
    Scalar bound(image->cols, image->rows, 1); // third dim size = 1
    // get all points on this line
    vector<Scalar> line_points = get_line_points(point, direction, bound);
    re.push_back((Scalar) *(line_points.begin()));
    // evaluate Scharr gradients of all points
    Scalar tmp_max; // max gradient point
    double tmp_g = -10000; // max gradient intensity
    bool neg = true; // previous point is not an edge point
    for (auto it = line_points.begin(); it != line_points.end(); ++it) {
        Scalar pt = *it;
        double grad = cv_img_point_scharr(image, pt);
        //cout << pt[0] << ", " << pt[1] << " : " << grad << endl;
        if (grad >= grad_threshold) {
            if (grad > tmp_g) {
                tmp_max = pt;
                tmp_g = grad;
            }
            neg = false;
        } else {
            if (!neg) {
                re.push_back(tmp_max);
                tmp_g = -100000;
            }
            neg = true;
        }
    }
    re.push_back((Scalar) *line_points.rbegin());
    return re;
}

vector<Scalar> cv_line_pts_scharr_geq_T(vector<Mat> *images,
                                        Scalar point,
                                        Scalar direction,
                                        double grad_threshold) {
    vector<Scalar> re;
    // size of the 3-d space
    Scalar bound((*images)[0].cols, (*images)[0].rows, images->size());
    // get all points on this line
    vector<Scalar> line_points = get_line_points(point, direction, bound);
    re.push_back((Scalar) *(line_points.begin()));
    // evaluate Scharr gradients of all points
    Scalar tmp_max; // max gradient point
    double tmp_g = -10000; // max gradient intensity
    bool neg = true; // previous point is not an edge point
    for (auto it = line_points.begin(); it != line_points.end(); ++it) {
        Scalar pt = *it;
        double grad = cv_imgs_point_scharr(images, pt);
        //cout << pt[0] << ", " << pt[1] << " : " << grad << endl;
        if (grad >= grad_threshold) {
            if (grad > tmp_g) {
                tmp_max = pt;
                tmp_g = grad;
            }
            neg = false;
        } else {
            if (!neg) {
                re.push_back(tmp_max);
                tmp_g = -100000;
            }
            neg = true;
        }
    }
    /*
    Scalar tmp_last; // temporary last point
    Scalar tmp_max; // max gradient point
    double tmp_g; // max gradient intensity
    bool cont = false; // flag of continuous component existence
    for (auto it = line_points.begin(); it != line_points.end(); ++it) {
        Scalar pt = *it;
        double grad = cv_imgs_point_scharr(images, pt);
        if (grad >= grad_threshold) {
            if (!cont) {
                tmp_last = pt;
                tmp_max = pt;
                tmp_g = grad;
                cont = true;
            } else {
                if (point_cont(tmp_last, pt)) {
                    tmp_last = pt;
                    //re.push_back(pt);
                    if (grad >= tmp_g) {
                        tmp_g = grad;
                        tmp_max = pt;
                    }
                } else { // when continuous ends
                    re.push_back(tmp_max);
                    cont = false;
                }
            }
        }
    }
    */
    re.push_back((Scalar) *line_points.rbegin());
    return re;
}

vector<Scalar> cv_line_seg_pts_scharr_geq_T(vector<Mat> *images,
                                            Scalar start,
                                            Scalar end,
                                            double grad_threshold) {
    vector<Scalar> re;
    // size of the 3-d space
    Scalar bound((*images)[0].cols, (*images)[0].rows, images->size());
    // get all points on this line
    vector<Scalar> line_points = get_line_seg_points(start, end, bound);
    re.push_back((Scalar) *(line_points.begin()));
    // evaluate Scharr gradients of all points
    Scalar tmp_max; // max gradient point
    double tmp_g = -10000; // max gradient intensity
    bool neg = true; // previous point is not an edge point
    for (auto it = line_points.begin(); it != line_points.end(); ++it) {
        Scalar pt = *it;
        double grad = cv_imgs_point_scharr(images, pt);
        //cout << pt[0] << ", " << pt[1] << " : " << grad << endl;
        if (grad >= grad_threshold) {
            if (grad > tmp_g) {
                tmp_max = pt;
                tmp_g = grad;
            }
            neg = false;
        } else {
            if (!neg) {
                re.push_back(tmp_max);
                tmp_g = -100000;
            }
            neg = true;
        }
    }
    /*
    Scalar tmp_last; // temporary last point
    Scalar tmp_max; // max gradient point
    double tmp_g; // max gradient intensity
    bool cont = false; // flag of continuous component existence
    for (auto it = line_points.begin(); it != line_points.end(); ++it) {
        Scalar pt = *it;
        double grad = cv_imgs_point_scharr(images, pt);
        if (grad >= grad_threshold) {
            if (!cont) {
                tmp_last = pt;
                tmp_max = pt;
                tmp_g = grad;
                cont = true;
            } else {
                if (point_cont(tmp_last, pt)) {
                    tmp_last = pt;
                    //re.push_back(pt);
                    if (grad >= tmp_g) {
                        tmp_g = grad;
                        tmp_max = pt;
                    }
                } else { // when continuous ends
                    re.push_back(tmp_max);
                    cont = false;
                }
            }
        }
    }
    */
    re.push_back((Scalar) *line_points.rbegin());
    return re;
}

double compare_hist(vector<Mat> *images,
                    vector<Scalar> points_1,
                    vector<Scalar> points_2) {
    // get colors
    vector<Scalar> colors_1;
    vector<Scalar> colors_2;
    for (auto it = points_1.begin(); it != points_1.end(); ++it)
        colors_1.push_back(cv_imgs_point_color_loc(images, (Scalar) *it,
                                                   Scalar(0, 0, 0)));
    for (auto it = points_2.begin(); it != points_2.end(); ++it)
        colors_2.push_back(cv_imgs_point_color_loc(images, (Scalar) *it,
                                                   Scalar(0, 0, 0)));
    // calculate histogram
    /* cout << "pts1:" << endl; */
    arma::Mat<int> freq_1(3, 32, arma::fill::zeros); // frequency
    arma::Mat<int> freq_2(3, 32, arma::fill::zeros);
    for (auto it = colors_1.begin(); it != colors_1.end(); ++it) {
        Scalar lab = *it;
        for (int channel = 0; channel < 3; channel++) {
            int f = lab[channel]/8;
            /*
            cout << channel << " .. "
                 << lab[channel] << " ~= " 
                 << f << endl;
            */
            freq_1(channel, f) = freq_1(channel, f) + 1;
        }
    }
    /* cout << "pts2:" << endl; */
    for (auto it = colors_2.begin(); it != colors_2.end(); ++it) {
        Scalar lab = *it;
        for (int channel = 0; channel < 3; channel++) {
            int f = lab[channel]/8;
            /*
            cout << channel << " .. "
                 << lab[channel] << " ~= " 
                 << f << endl;
            */
            freq_2(channel, f) = freq_2(channel, f) + 1;
        }
    }
    arma::mat hist_1(3, 32, arma::fill::zeros); // histogram
    arma::mat hist_2(3, 32, arma::fill::zeros);
    /* cout << "frequencies: " << endl; */
    for (int f = 0; f < 32; f++) {
        for (int ch = 0; ch < 3; ch++) {
            /* cout << freq_1(ch, f) << "/" << freq_2(ch, f) << ", "; */
            // smooth the distribution with Dirichlet prior
            hist_1(ch, f) =
                (freq_1(ch, f) + 0.0001)/(arma::sum(freq_1.row(ch)) + 0.0032);
            hist_2(ch, f) =
                (freq_2(ch, f) + 0.0001)/(arma::sum(freq_2.row(ch)) + 0.0032);
        }
        /* cout << endl; */
    }
    // calculate KL divergence
    Scalar kls(0.0, 0.0, 0.0);
    for (int ch = 0; ch < 3; ch++) {
        double D_1_2 = 0;
        double D_2_1 = 0;
        for (int f = 0; f < 32; f++) {
            D_1_2 += hist_1(ch, f)*log2(hist_1(ch, f)/hist_2(ch, f));
            D_2_1 += hist_2(ch, f)*log2(hist_2(ch, f)/hist_1(ch, f));
        }
        kls[ch] = (D_1_2 + D_2_1)/2;
        /* cout << kls[ch] << " "; */
    }
    // quadratic mean of 3 channels
    double kl = sqrt((kls[0]*kls[0] + kls[1]*kls[1] + kls[2]*kls[2])/3);
    /* cout << ".......... " << kl << endl; */
    return kl;
}

vector<double> cv_points_color_hist(vector<Mat> *images,
                                    vector<Scalar> points) {
    // get colors
    vector<Scalar> colors;
    for (auto it = points.begin(); it != points.end(); ++it)
        colors.push_back(cv_imgs_point_color_loc(images, (Scalar) *it,
                                                 Scalar(0, 0, 0)));
    // frequency
    arma::Mat<int> freq(3, 32, arma::fill::zeros); 
    for (auto it = colors.begin(); it != colors.end(); ++it) {
        Scalar lab = *it;
        for (int channel = 0; channel < 3; channel++) {
            int f = lab[channel]/8;
            freq(channel, f) = freq(channel, f) + 1;
        }
    }
    // calculate distribution
    vector<double> re;
    for (int f = 0; f < 32; f++)
        for (int ch = 0; ch < 3; ch++)
            re.push_back((freq(ch, f) + 0.0001)
                         / (arma::sum(freq.row(ch)) + 0.0032));
    return re;
}

#endif
