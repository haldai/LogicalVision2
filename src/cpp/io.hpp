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

        // convert to LAB space (comparing to
        //     RGB color space, Lab is closer to human cognition)
        cvtColor(frame, frame, COLOR_BGR2Lab); 
        seq->push_back(frame.clone());
        ++frame_current;
    }
    /* debug start
    Mat frame_1 = (*seq)[0];
    Vec3b s = frame_1.at<Vec3b>(1000, 100);
    cout << (int) s[0] << ", "
         << (int) s[1] << ", "
         << (int) s[2] << endl;
    
    
    double frame_rate = vid->get(CV_CAP_PROP_FPS);
    namedWindow("debug");
    int delay = 1000/frame_rate;
    for (auto it = seq->begin(); it != seq->end() && !stop; ++it) {
        frame = *it;
        //Mat frame_;
        cvtColor(frame, frame, COLOR_Lab2BGR);
        imshow("debug", frame);
        int c = waitKey(delay);
        if((char) c == 27 || frame_current > frame_total)
            stop = true;
        else if(c >= 0)
            waitKey(0);
    }
    destroyWindow("debug");
    */ 
    return seq;
}

#endif
