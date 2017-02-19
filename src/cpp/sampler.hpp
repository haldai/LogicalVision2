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
along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
************************************************************************/
/* Sampling module
 * ================================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

#ifndef _SAMPLER_HPP
#define _SAMPLER_HPP
#include "utils.hpp"
#include "geometry.hpp"

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

/* compute the color histogram of a set of points in image sequence
 * @images: image sequence
 * @points: point set
 */
vector<double> cv_points_color_hist(vector<Mat> *images,
                                    vector<Scalar> points);

/* get color histogram feature (output Armadillo column vector)
 * @image: pointer to opencv image
 * @rect: Scalar(X)
 */
vector<double> cv_rect_masked_color_hist_2d(cv::Mat *image,
                                            cv::Scalar center,
                                            cv::Scalar radius);
vector<double> cv_rect_masked_L_hist_2d(cv::Mat *image,
                                        cv::Scalar center,
                                        cv::Scalar radius);

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

vector<double> cv_rect_masked_color_hist_2d(cv::Mat *image,
                                            cv::Scalar center,
                                            cv::Scalar radius) {
    vector<double> re_vec;
    // create mask
    cv::Mat mask = cv::Mat::zeros(image->size(), CV_8U);
    int w = image->cols;
    int h = image->rows;
    int x = center[0] - radius[0];
    int y = center[1] - radius[1];
    int lx = 2*radius[0] + 1;
    int ly = 2*radius[1] + 1;
    if (x <= 0) {
        lx = lx + x;
        x = 0;
    }
    if (y <= 0) {
        ly = ly + y;
        y = 0;
    }
    if (x + lx >= w)
        lx = w - x - 1;
    if (y + ly >= h)
        ly = h - y - 1;
    if (lx <= 0 || ly <= 0)
        return re_vec;
    /*
    cout << x << ","
         << y << ","
         << lx << ","
         << ly << "," << endl;
    */
    Mat roi(mask, cv::Rect(x, y, lx, ly));
    roi = 1;
    /*
    cv::Mat roiImg;
    image->copyTo(roiImg, mask);
    cvtColor(roiImg, roiImg, COLOR_Lab2BGR);
    imshow("roi", roiImg);
    cv::waitKey(0);
    */

    // calculate historgram
    cv::Mat hist;
    int lsize = 4;
    int asize = 4;
    int bsize = 4;
    int histSize[] = {lsize, asize, bsize};
    float range[] = {0, 256};
    const float* ranges[] = {range, range, range};

    int channels[] = {0, 1, 2};
    cv::calcHist(image, 1, channels, mask, // use mask
                 hist, 3, histSize, ranges,
                 true, // the histogram is uniform
                 false);
    cv::normalize(hist, hist, 1, 0, cv::NORM_L1); // normalization
    for (auto l = 0; l < lsize; l++)
        for (auto a = 0; a < asize; a++)
            for (auto b = 0; b < bsize; b++)
                re_vec.push_back((double) hist.at<float>(l, a, b));

    return re_vec;
}

vector<double> cv_rect_masked_L_hist_2d(cv::Mat *image,
                                        cv::Scalar center,
                                        cv::Scalar radius) {
    vector<double> re_vec;
    // create mask
    cv::Mat mask = cv::Mat::zeros(image->size(), CV_8U);
    int w = image->cols;
    int h = image->rows;
    int x = center[0] - radius[0];
    int y = center[1] - radius[1];
    int lx = 2*radius[0] + 1;
    int ly = 2*radius[1] + 1;
    if (x <= 0) {
        lx = lx + x;
        x = 0;
    }
    if (y <= 0) {
        ly = ly + y;
        y = 0;
    }
    if (x + lx >= w)
        lx = w - x - 1;
    if (y + ly >= h)
        ly = h - y - 1;
    if (lx <= 0 || ly <= 0)
        return re_vec;

    /*
    cout << x << ","
         << y << ","
         << lx << ","
         << ly << "," << endl;
    */
    Mat roi(mask, cv::Rect(x, y, lx, ly));
    roi = 1;
    /*
    cv::Mat roiImg;
    image->copyTo(roiImg, mask);
    cvtColor(roiImg, roiImg, COLOR_Lab2BGR);
    imshow("roi", roiImg);
    cv::waitKey(0);
    */

    // calculate historgram
    cv::Mat hist;
    int lsize = 256;
    int histSize[] = {lsize};
    float range[] = {0, 256};
    const float* ranges[] = {range};

    int channels[] = {0};
    cv::calcHist(image, 1, channels, mask, // use mask
                 hist, 1, histSize, ranges,
                 true, // the histogram is uniform
                 false);
    cv::normalize(hist, hist, 1, 0, cv::NORM_L1); // normalization
    for (auto l = 0; l < lsize; l++)
        re_vec.push_back((double) hist.at<float>(l));

    return re_vec;
}

#endif
