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
/* Drawing
 * ================================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

#ifndef _DRAW_HPP_
#define _DRAW_HPP_

#include <opencv2/core/core.hpp>
#include <opencv2/highgui.hpp>
#include <opencv2/imgproc.hpp>

#include <iostream> // for standard I/O

#define BLACK Scalar(0, 128, 128)
#define BLUE Scalar(0, 128, 0)
#define GREEN Scalar(128, 0, 255)
#define RED Scalar(128, 255, 255)
#define YELLOW Scalar(255, 128, 255)
#define WHITE Scalar(255, 128, 128)

using namespace std;
using namespace cv;

/********* declaration *********/
// draw a line on img
void cv_draw_line(Mat img, Point start, Point end, Scalar color);

// draw a point on img
void cv_draw_point(Mat img, Point point, Scalar color);

// draw a circle on img
void cv_draw_circle(Mat img, Point point, double radius, Scalar color, int thickness = -1);

void cv_draw_ellipse(Mat img, Point center, Scalar Param, Scalar color, int thickness = 1);

// draw a rectangle on img
void cv_draw_rect(Mat img, Scalar center, Scalar radius, Scalar color, int thickness = 1);

/********* implementation *********/
void cv_draw_line(Mat img, Point start, Point end, Scalar color) {
    int thickness = 2;
    int lineType = 8;
    line(img, start, end, color, thickness, lineType);
}

void cv_draw_point(Mat img, Point point, Scalar color) {
    cv_draw_circle(img, point, 1, color);
}

void cv_draw_circle(Mat img, Point point, double radius, Scalar color, int thickness) {
    int lineType = 8;
    circle(img, point, radius, color, thickness, lineType);
}

void cv_draw_ellipse(Mat img, Point center, Scalar Param, Scalar color, int thickness) {
    int lineType = 8;
    ellipse(img, center, Size(Param[0], Param[1]), Param[2], 0, 360, color, thickness, lineType);
}

void cv_draw_rect(Mat img, Scalar center, Scalar radius, Scalar color, int thickness) {
    int lineType = 8;
    int x = (center[0] - radius[0] >= 0) ? (center[0] - radius[0]) : 0;
    int y = (center[1] - radius[1] >= 0) ? (center[1] - radius[1]) : 0;
    int lx = 2*radius[0] + 1;
    int ly = 2*radius[1] + 1;
    Rect rec(x, y, lx, ly);
    rectangle(img, rec,	color, thickness, lineType);
}
#endif
