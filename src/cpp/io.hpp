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
/* IO for media type
 *     Store all contents in stack
 * ================================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

#ifndef _IO_HPP
#define _IO_HPP

#include "memread.hpp"

#include <opencv2/core/core.hpp>
#include <opencv2/imgproc.hpp>
#include <opencv2/videoio/videoio.hpp>
#include <opencv2/highgui.hpp>  // OpenCV window I/O
#include <SWI-cpp.h>
#include <SWI-Prolog.h>

#include <iostream> // for standard I/O
#include <string>   // for strings

using namespace std;
using namespace cv;

/********** declaration ***********/
// load an image into stack and transform to Lab color
Mat* cv_load_img(string path);
// load an video into stack
VideoCapture *cv_load_video(string path);
// transform video as an image sequence and transform to Lab color
vector<Mat> *cv_video2imgseq(VideoCapture *vid);
vector<Mat> *cv_video2greyseq(VideoCapture *vid); // to greyscale
// get subimages
Mat* cv_get_subimage(Mat *img, Scalar roi);
// resize image
Mat* cv_resize_image(Mat *img, int width, int height);

/*********** implementation ************/
Mat* cv_load_img(string path) {
    Mat *img = new Mat();
    *img = imread(path, IMREAD_COLOR);
    cvtColor(*img, *img, COLOR_BGR2Lab); 
    return img;
}

VideoCapture *cv_load_video(string path) {
    VideoCapture *vid = new VideoCapture();
    vid->open(path, CAP_FFMPEG);
    if(!vid->isOpened()) {
        cout << "Failed to open!" << endl;
        return NULL;
    }
    return vid;
}

vector<Mat> *cv_video2imgseq(VideoCapture *vid) {
    long frame_total = vid->get(CV_CAP_PROP_FRAME_COUNT);
    vid->set(CAP_PROP_POS_FRAMES, 0); // set the read point to the beginning
    bool stop = false;
    Mat frame;
    vector<Mat> *seq = new vector<Mat>;
    long frame_current = 0;
    
    while(!stop && frame_current < frame_total) {
        if(!vid->read(frame)) {
            cout << "Reading frame " << frame_current
                 << " failed" << endl;
            return FALSE;
        }
        // medianBlur(frame, frame, 5);
        // convert to LAB space (comparing to
        //     RGB color space, Lab is closer to human cognition)
        cvtColor(frame, frame, COLOR_BGR2Lab);
        seq->push_back(frame.clone());
        ++frame_current;
    }
    return seq;
}

vector<Mat> *cv_video2greyseq(VideoCapture *vid) {
    long frame_total = vid->get(CV_CAP_PROP_FRAME_COUNT);
    vid->set(CAP_PROP_POS_FRAMES, 0); // set the read point to the beginning
    bool stop = false;
    Mat frame;
    vector<Mat> *seq = new vector<Mat>;
    long frame_current = 0;
    
    while(!stop && frame_current < frame_total) {
        if(!vid->read(frame)) {
            cout << "Reading frame " << frame_current
                 << " failed" << endl;
            return FALSE;
        }

          // convert to LAB space (comparing to
        //     RGB color space, Lab is closer to human cognition)
        cvtColor(frame, frame, COLOR_BGR2GRAY);
        seq->push_back(frame.clone());
        ++frame_current;
    }
    return seq;
}

Mat* cv_get_subimage(Mat *img, Scalar roi) {
    int x = roi[0];
    int y = roi[1];
    int rx = roi[2];
    int ry = roi[3];
    
    x = (x - rx >= 0) ? (x - rx) : 0;
    y = (x - ry >= 0) ? (x - ry) : 0;
    int lx = 2*rx + 1;
    int ly = 2*ry + 1;
    
    Rect r(x, y, lx, ly);
    Mat *re = new Mat((*img)(r).clone());
    return re;
}

Mat* cv_resize_image(Mat *img, int width, int height) {
    Mat *re = new Mat(cv::Size(width, height), img->type());
    cv::resize(*img, *re, re->size(), 0, 0, cv::INTER_AREA);
    return re;
}

#endif
