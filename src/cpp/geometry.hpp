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
#ifndef _GEOMETRY_HPP
#define _GEOMETRY_HPP
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
#define PI 3.14159265358979323846

enum { XY_SHIFT = 16,
       XY_ONE = 1 << XY_SHIFT,
       DRAWING_STORAGE_BLOCK = (1 << 12) - 256 };

/* get all points on a line
 * @point: position of a point on the line
 * @direction: direction of the line
 * @bound: size of the 3d-space
 * @return: all points on the line
 */
vector<Scalar> get_line_points(Scalar point, Scalar direction, Scalar bound);
vector<Scalar> get_ray_points(Scalar point, Scalar direction, Scalar bound);

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

/* fits a set of points with an ellipse on an 2d image
 * Based on:
 * NUMERICALLY STABLE DIRECT LEAST SQUARES FITTING OF ELLIPSES
 *  Radim Halir and Jan Flusser, WSCG98
 * @points: points for fitting
 * @centre: center of the ellipse
 * @param: other parameters (long/short axis length and axis angle)
 */
int fit_ellipse_2d(const vector<Scalar> & points,
                    Scalar & center,
                    Scalar & param);
void opencv_fit_ellipse(const vector<Scalar> & points,
                        Scalar & centre,
                        Scalar & param);

/* fits a set of points with a circle on an 2d image
 * @points: points for fitting
 * @param: parameter of the circle (centerX, centerY, radius)
 */
void fit_circle_2d(const vector<Scalar> & points, Scalar& param);

/* Pseudocode for robustly computing the closest ellipse point and
 * distance to a query point. It is required that e0 >= e1 > 0, y0 >= 0,
 * and y1 >= 0.
 * Ref: https://www.geometrictools.com/Documentation/DistancePointEllipseEllipsoid.pdf
 * @e0, e1: ellipse dimension 0 and 1, where 0 is greater and both are positive.
 * @y0, y1: initial point on ellipse axis (center of ellipse is 0,0)
 * @x0, x1: intersection point
 */
double GetRoot (double r0, double z0, double z1, double g);
double DistancePointEllipse(cv::Point point,
                            cv::Point2i center, Scalar param,
                            cv::Point & closest);

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

/****************** implementations *********************/
void opencv_fit_ellipse(const vector<Scalar> & points,
                        Scalar& centre,
                        Scalar& param) {
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
    // we need radius = half of them
    int a = round(box.size.width/2), b = round(box.size.height/2);
    int alpha = round(box.angle);
    if (a < b) {
        SWAP(a, b);
        alpha -= 90;
    }
    
    param = Scalar(a, b, alpha);
}

int fit_ellipse_2d(const vector<Scalar> & points,
                    Scalar & center,
                    Scalar & param) {
    arma::vec x(points.size()), y(points.size());

    for (size_t i = 0; i < points.size(); i++) {
        x.at(i) = points[i][0];
        y.at(i) = points[i][1];
    }
    //double mean_x = mean(x), mean_y = mean(y); // mean?
    //x = x - mean_x;
    //y = y - mean_y;
    arma::vec xx = x % x;
    arma::vec xy = x % y;
    arma::vec yy = y % y;
    arma::vec o(points.size());
    o.ones();
    arma::mat d1 = arma::join_rows(arma::join_rows(xx, xy), yy);
    arma::mat d2 = arma::join_rows(arma::join_rows(x, y), o);

    arma::mat s1 = d1.t()*d1;
    arma::mat s2 = d1.t()*d2;
    arma::mat s3 = d2.t()*d2;
    arma::mat t = - solve(s3, s2.t());
    arma::mat m = s1 + s2*t;
    arma::rowvec m1 = m.row(2)/2;
    arma::rowvec m2 = - m.row(1);
    arma::rowvec m3 = m.row(0)/2;
    m = arma::join_cols(arma::join_cols(m1, m2), m3);

    arma::cx_vec eigval;
    arma::cx_mat eigvec;
    // solving eigen problem
    try {
        eig_gen(eigval, eigvec, m);
    } catch (...) {
        cout << "[fit_ellipse_2d] Solving eigen value failed!" << endl;
        center = Scalar(0, 0, 0);
        param = Scalar(100, 100, 0); // show error by display
        return 0;
    }
    
    arma::vec eval = real(eigval);
    arma::mat evec = real(eigvec);

    arma::rowvec cond = 4*(evec.row(0)%evec.row(2)) - evec.row(1)%evec.row(1);
    arma::uvec q = find(cond > 0);
    arma::mat a1 = evec.cols(q); // sometimes a1 = []
    arma::mat a_vec = arma::join_cols(a1, t*a1);
    if (a_vec.is_empty()) {
        cout << "[fit_ellipse_2d] Degenerated result!" << endl;        
        center = Scalar(0, 0, 0);
        param = Scalar(100, 100, 0); // show error by display
        return 0;
    }

    double a = a_vec.at(0);
    double b = a_vec.at(1)/2;
    double c = a_vec.at(2);
    double d = a_vec.at(3)/2;
    double f = a_vec.at(4)/2;
    double g = a_vec.at(5);

    if (b*b - a*c == 0 || a == c) { // degenerated ellipse
        cout << "[fit_ellipse_2d] Degenerated result!" << endl;        
        center = Scalar(0, 0, 0);
        param = Scalar(100, 100, 0); // show error by display
        return 0;
    }
    
    double x0 = (c*d - b*f)/(b*b - a*c);
    double y0 = (a*f - b*d)/(b*b - a*c);

    double axis_a = (2*(a*f*f + c*d*d + g*b*b - 2*b*d*f - a*c*g))
        / ((b*b - a*c)*(sqrt((a - c)*(a - c) + 4*b*b) - (a + c)));
    double axis_b = (2*(a*f*f + c*d*d + g*b*b - 2*b*d*f - a*c*g))
        / ((b*b - a*c)*(- sqrt((a - c)*(a - c) + 4*b*b) - (a + c)));
    axis_a = sqrt(axis_a);
    axis_b = sqrt(axis_b);

    
    double phi; // degree
    if (b == 0 && a < c)
        phi = 0;
    else if (b == 0 && a > c)
        phi = 90;
    else if (b != 0 && a < c)
        phi = (0.5*atan(2*b/(a - c)))*180/PI;
    else
        phi = 90 + (0.5*atan(2*b/(a - c)))*180/PI;
    /*
    if (axis_a < axis_b) {
        //SWAP(axis_a, axis_b);
        phi = phi - 90;
    }
    */

    center = Scalar(round(x0), round(y0), 0);
    param = Scalar(round(axis_a), round(axis_b),
                   round(phi));
    return 1;
}


void fit_circle_2d(const vector<Scalar> & points,
                   Scalar& param) {
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

// 3D Bresenham's line generation
vector<Scalar> get_line_points(Scalar point,
                               Scalar direction,
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
    if (!out_of_canvas(point, bound))
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

vector<Scalar> get_ray_points(Scalar point, Scalar direction, Scalar bound) {
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
    re.push_back(point); // insert itself
    bresenham(point, direction, inc, bound, &re);
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
    if (!out_of_canvas(Scalar(x2, y2, z2), bound))
        re.push_back(Scalar(x2, y2, z2));
    return re;
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

double GetRoot (double r0, double z0, double z1, double g) {
    double n0 = r0*z0;
    double s0 = z1 - 1;
    double s1 = (g < 0 ? 0 : sqrt(n0*n0+z1*z1) - 1);
    double s = 0;
    for (int i = 0; i < 50; i++) {
        s = (s0 + s1) / 2 ;
        if (s == s0 || s == s1)
            break;
        double ratio0 = n0 /( s + r0 );
        double ratio1 = z1 /( s + 1 );
        g = ratio0*ratio0 + ratio1*ratio1 - 1;
        if (g > 1e-5)
            s0 = s;
        else if (g < -1e-5)
            s1 = s;
        else
            break;
    }
    return s;
}

double DistancePointEllipse(cv::Point point,
                            cv::Point2i center, Scalar param,
                            cv::Point & closest) {
//    double e0, double e1, double y0, double y1,
//                            double & x0, double & x1) {
    double y0_ = (double) point.x, y1_ = (double) point.y;
    double c0 = (double) center.x, c1 = (double) center.y;
    double e0 = param[0], e1 = param[1];    
    double x0, x1;
    bool neg_x = false, neg_y = false;
    // tilting and moving
    double theta;
    if (e0 < e1) {
        SWAP(e0, e1);
        theta = (param[2] + 90)*PI/180;
    } else
        theta = param[2]*PI/180;
    double y0 = (y0_ - c0)*cos(theta) + (y1_ - c1)*sin(theta);
    double y1 = - (y0_ - c0)*sin(theta) + (y1_ - c1)*cos(theta);

    // change quadrant
    if (abs(y0) < 1e-5)
        y0 = 0.0;
    if (abs(y1) < 1e-5)
        y1 = 0.0;
    if (y0 < 0) {
        neg_x = true;
        y0 = - y0;
    };
    if (y1 < 0) {
        neg_y = true;
        y1 = - y1;
    };

    double distance;
    if (y1 > 0) {
        if (y0 > 0) {
            double z0 = y0 / e0; 
            double z1 = y1 / e1; 
            double g = z0*z0 + z1*z1 - 1;
            if (g != 0) {
                double r0 = (e0/e1)*(e0/e1);
                double sbar = GetRoot(r0 , z0 , z1 , g);
                x0 = r0*y0/(sbar + r0);
                x1 = y1/(sbar + 1);
                distance = sqrt((x0 - y0)*(x0 - y0) + (x1 - y1)*(x1 - y1));
            } else {
                x0 = y0; 
                x1 = y1;
                distance = 0;
            }
        } else {// y0 == 0
            x0 = 0;
            x1 = e1;
            distance = abs(y1 - e1);
        }
    } else { // y1 == 0
        double numer0 = e0*y0, denom0 = e0*e0 - e1*e1;
        if (numer0 < denom0) {
            double xde0 = numer0/denom0;
            x0 = e0*xde0;
            x1 = e1*sqrt(1 - xde0*xde0);
            distance = sqrt( (x0-y0)*(x0-y0) + x1*x1);
        } else {
            x0 = e0; 
            x1 = 0; 
            distance = abs(y0 - e0);
        }
    }
    // change quadrant
    if (neg_x)
        x0 = - x0;
    if (neg_y)
        x1 = - x1;

    // tilt back
    double x0_ = x0*cos(theta) - x1*sin(theta);
    double x1_ = x0*sin(theta) + x1*cos(theta);
    // moving
    closest.x = round(x0_ + c0);
    closest.y = round(x1_ + c1);
    return distance;
}

#endif

