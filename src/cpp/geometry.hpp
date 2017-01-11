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
 *   Fitzgibbon, A.W., Pilu, M., and Fischer R.B., Direct least squares
 *   fitting of ellipsees, Proc. of the 13th Internation Conference on Pattern
 *   Recognition, pp 253–257, Vienna, 1996.
 * @points: points for fitting
 * @centre: center of the ellipse
 * @param: other parameters (long/short axis length and axis angle)
 */
void opencv_fit_ellipse(const vector<Scalar> & points,
                        Scalar & centre,
                        Scalar & param);

/* fits a set of points with a circle on an 2d image
 * @points: points for fitting
 * @param: parameter of the circle (centerX, centerY, radius)
 */
void fit_circle_2d(const vector<Scalar> & points, Scalar& param);

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
    param = Scalar(round(box.size.width/2), // we need radius = half of them
                   round(box.size.height/2),
                   box.angle);
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

#endif
