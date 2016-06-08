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

#define BLACK Scalar(0, 128, 128)
#define BLUE Scalar(0, 128, 0)
#define GREEN Scalar(255, 0, 255)
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

/********* implementation *********/
void cv_draw_line(Mat img, Point start, Point end, Scalar color) {
    int thickness = 2;
    int lineType = 8;
    line(img, start, end, color, thickness, lineType);
}

void cv_draw_point(Mat img, Point point, Scalar color) {
    cv_draw_circle(img, point, 2, color);
}

void cv_draw_circle(Mat img, Point point, double radius, Scalar color, int thickness) {
    int lineType = 8;
    circle(img, point, radius, color, thickness, lineType);
}

void cv_draw_ellipse(Mat img, Point center, Scalar Param, Scalar color, int thickness) {
    int lineType = 8;
    ellipse(img, center, Size(Param[0], Param[1]), Param[2], 0, 360, color, thickness, lineType);
}

#endif
