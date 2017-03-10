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
/* IO library for prolog
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

#include "utils.hpp"
#include "io.hpp"
#include "memread.hpp"
#include "errors.hpp"
#include "superpixel/super_pixel.hpp"

//#include <opencv2/core/core.hpp>
#include <opencv2/imgproc.hpp>
#include <opencv2/videoio/videoio.hpp>
#include <opencv2/highgui.hpp>  // OpenCV window I/O
#include <SWI-cpp.h>
#include <SWI-Prolog.h>

#include <string>

using namespace std;
using namespace cv;

/* load_img(PATH, ADD)
 * load image from PATH into stack ADD
 * also automaticall assert a size_2d(ADD, width, height) of the image
 */
PREDICATE(load_img, 2) {
    term_t t1 = A1.ref;
    char *p1;
    if (PL_get_atom_chars(t1, &p1)) {
        const string path(p1);
        Mat* img = cv_load_img(path);
        if (img->cols > 360 || img->rows > 360) {
            double t = 0;
            if (img->cols > img->rows)
                t = 360.0/img->cols;
            else
                t = 360.0/img->rows;
            Mat* rsz = cv_resize_image(img,
                                       round(t*(img->cols)),
                                       round(t*(img->rows)));
            delete img;
            img = rsz;
            rsz = NULL;
        }
        string add = ptr2str(img); // address of image in stack
        term_t t2 = PL_new_term_ref();
        if (PL_put_atom_chars(t2, add.c_str())) {
            A2 = PlTerm(t2);
            // assert size_2d
            int col = img->cols;
            int row = img->rows;
            PlTermv size_2d_args(3);
            size_2d_args[0] = A2;
            size_2d_args[1] = col; // width
            size_2d_args[2] = row; // height
            PlTermv size_2d_atom(1);
            size_2d_atom[0] = PlCompound("size_2d", size_2d_args);
            PlCall("assertz", size_2d_atom);
            return TRUE;
        } else
            return PUT_ERROR("load_img/2", 2, "ADD", "STRING");
    } else
        return LOAD_ERROR("load_img/2", 1, "PATH", "STRING");
}

/* save_img(ADD, PATH)
 * save image from ADD to PATH
 */
PREDICATE(save_img, 2) {
    char *p1;
    char *p2;
    if (!(p1 = (char*) A1) || !(p2 = (char*) A2))
        return FALSE;
    const string add_img(p1);
    Mat *img = str2ptr<Mat>(add_img);
    Mat frame = img->clone();
    cvtColor(frame, frame, COLOR_Lab2BGR);
    const string out_path(p2);
    try {
        imwrite(out_path, frame);
    } catch (cv::Exception& ex) {
        fprintf(stderr, "Exception converting image to PNG format: %s\n", ex.what());
        return FALSE;
    }
    return TRUE;
}

/* load_video(PATH, ADD)
 * load a video into stack
 * also automaticall assert size_2d(ADD, width, height)
 *    and size_3d(ADD, width, height, duration) of the video.
 */
PREDICATE(load_video, 2) {
    term_t t1 = A1.ref;
    char *p1;
    if (PL_get_atom_chars(t1, &p1)) {
        const string path(p1);        
        VideoCapture *vid = cv_load_video(path);
        if (vid == NULL)
            return FALSE;
        string add = ptr2str(vid); // address of video in stack
        // assert size_3d and size_2d
        term_t t2 = PL_new_term_ref();
        if (PL_put_atom_chars(t2, add.c_str())) {
            A2 = PlTerm(t2);
            int wid = vid->get(CAP_PROP_FRAME_WIDTH);
            int hei = vid->get(CAP_PROP_FRAME_HEIGHT);
            int dur = vid->get(CAP_PROP_FRAME_COUNT); // duration
            // 2d
            PlTermv size_2d_args(3);
            size_2d_args[0] = A2;
            size_2d_args[1] = wid;
            size_2d_args[2] = hei;
            PlTermv size_2d_atom(1);
            size_2d_atom[0] = PlCompound("size_2d", size_2d_args);
            // 3d
            PlTermv size_3d_args(4);
            size_3d_args[0] = A2;
            size_3d_args[1] = wid;
            size_3d_args[2] = hei;
            size_3d_args[3] = dur;
            PlTermv size_3d_atom(1);
            size_3d_atom[0] = PlCompound("size_3d", size_3d_args);
            // assertion
            PlCall("assertz", size_2d_atom);
            PlCall("assertz", size_3d_atom);
            return TRUE;
        } else
            return PUT_ERROR("load_video/2", 2, "ADD", "STRING");
    } else
        return LOAD_ERROR("load_video/2", 1, "PATH", "STRING");
}

/* video2imgseq(ADD_V, ADD_I)
 * transform the video (ADD_V) into a sequence of image (ADD_I)
 * the images are stored in a STL vector<Mat>
 */
PREDICATE(video2imgseq, 2) {
    term_t t1 = A1.ref;
    char *p1;
    if (PL_get_atom_chars(t1, &p1)) {
        const string add_v(p1);
        VideoCapture *vid = str2ptr<VideoCapture>(add_v);
        vector<Mat> *imgseq = cv_video2imgseq(vid);
        string add_i = ptr2str(imgseq);
        term_t t2 = PL_new_term_ref();

        // assert size_3d and size_2d
        if (PL_put_atom_chars(t2, add_i.c_str())) {
            A2 = PlTerm(t2);
            int wid = vid->get(CAP_PROP_FRAME_WIDTH);
            int hei = vid->get(CAP_PROP_FRAME_HEIGHT);
            int dur = vid->get(CAP_PROP_FRAME_COUNT); // duration
            // 2d
            PlTermv size_2d_args(3);
            size_2d_args[0] = A2;
            size_2d_args[1] = wid;
            size_2d_args[2] = hei;
            PlTermv size_2d_atom(1);
            size_2d_atom[0] = PlCompound("size_2d", size_2d_args);
            // 3d
            PlTermv size_3d_args(4);
            size_3d_args[0] = A2;
            size_3d_args[1] = wid;
            size_3d_args[2] = hei;
            size_3d_args[3] = dur;
            PlTermv size_3d_atom(1);
            size_3d_atom[0] = PlCompound("size_3d", size_3d_args);
            // assertion
            PlCall("assertz", size_2d_atom);
            PlCall("assertz", size_3d_atom);
            return TRUE;
        } else
            return PUT_ERROR("video2imgseq/2", 2, "ADD_I", "STRING");
    } else
        return LOAD_ERROR("video2imgseq/2", 1, "ADD_V", "STRING");
}

/* video2greyseq(ADD_V, ADD_I)
 * transform the video (ADD_V) into a sequence of grey scaled image (ADD_I)
 * the images are stored in a STL vector<Mat>
 */
PREDICATE(video2greyseq, 2) {
    term_t t1 = A1.ref;
    char *p1;
    if (PL_get_atom_chars(t1, &p1)) {
        const string add_v(p1);
        VideoCapture *vid = str2ptr<VideoCapture>(add_v);
        vector<Mat> *imgseq = cv_video2greyseq(vid);
        string add_i = ptr2str(imgseq);
        term_t t2 = PL_new_term_ref();

        // assert size_3d and size_2d
        if (PL_put_atom_chars(t2, add_i.c_str())) {
            A2 = PlTerm(t2);
            int wid = vid->get(CAP_PROP_FRAME_WIDTH);
            int hei = vid->get(CAP_PROP_FRAME_HEIGHT);
            int dur = vid->get(CAP_PROP_FRAME_COUNT); // duration
            // 2d
            PlTermv size_2d_args(3);
            size_2d_args[0] = A2;
            size_2d_args[1] = wid;
            size_2d_args[2] = hei;
            PlTermv size_2d_atom(1);
            size_2d_atom[0] = PlCompound("size_2d", size_2d_args);
            // 3d
            PlTermv size_3d_args(4);
            size_3d_args[0] = A2;
            size_3d_args[1] = wid;
            size_3d_args[2] = hei;
            size_3d_args[3] = dur;
            PlTermv size_3d_atom(1);
            size_3d_atom[0] = PlCompound("size_3d", size_3d_args);
            // assertion
            PlCall("assertz", size_2d_atom);
            PlCall("assertz", size_3d_atom);
            return TRUE;
        } else
            return PUT_ERROR("video2imgseq/2", 2, "ADD_I", "STRING");
    } else
        return LOAD_ERROR("video2imgseq/2", 1, "ADD_V", "STRING");
}

/* release_img(ADD)
 * release image ADD in stack and retract size info
 */
PREDICATE(release_img, 1) {
    term_t t1 = A1.ref;
    char *p1;
    if (PL_get_atom_chars(t1, &p1)) {
        string add(p1);
        Mat* img = str2ptr<Mat>(add);
        // retract size_2d
        PlTermv size_2d_args(3);
        size_2d_args[0] = A1;
        PlTermv size_2d_atom(1);
        size_2d_atom[0] = PlCompound("size_2d", size_2d_args);
        PlCall("retractall", size_2d_atom);
        // remove img from memory
        delete img;
        return TRUE;
    } else
        return LOAD_ERROR("release_img/1", 1, "ADD", "STRING");
}

/* release_video(ADD)
 * release a video in stack and retract size info
 */
PREDICATE(release_video, 1) {
    term_t t1 = A1.ref;
    char *p1;
    if (PL_get_atom_chars(t1, &p1)) {
        string add(p1);
        VideoCapture *vid = str2ptr<VideoCapture>(add);
        // retract size_2d
        PlTermv size_2d_args(3);
        size_2d_args[0] = A1;
        PlTermv size_2d_atom(1);
        size_2d_atom[0] = PlCompound("size_2d", size_2d_args);
        PlCall("retractall", size_2d_atom);
        // retract size_3d
        PlTermv size_3d_args(4);
        size_3d_args[0] = A1;
        PlTermv size_3d_atom(1);
        size_3d_atom[0] = PlCompound("size_3d", size_3d_args);
        PlCall("retractall", size_3d_atom);
        // remove video from memory
        vid->release();
        delete vid;
        return TRUE;
    } else
        return LOAD_ERROR("release_video/1", 1, "ADD", "STRING");
}

/* release_imgseq(ADD)
 * release an image sequence in stack
 */
PREDICATE(release_imgseq, 1) {
    term_t t1 = A1.ref;
    char *p1;
    if (PL_get_atom_chars(t1, &p1)) {
        const string add(p1);
        vector<Mat> *imgseq = str2ptr<vector<Mat>>(add);
        // retract size_2d
        PlTermv size_2d_args(3);
        size_2d_args[0] = A1;
        PlTermv size_2d_atom(1);
        size_2d_atom[0] = PlCompound("size_2d", size_2d_args);
        PlCall("retractall", size_2d_atom);
        // retract size_3d
        PlTermv size_3d_args(4);
        size_3d_args[0] = A1;
        PlTermv size_3d_atom(1);
        size_3d_atom[0] = PlCompound("size_3d", size_3d_args);
        PlCall("retractall", size_3d_atom);
        // remove image sequence from memory
        delete imgseq;
        return TRUE;
    } else
        return LOAD_ERROR("release_video/1", 1, "ADD", "STRING");
}

/* showimg_win(ADD, WINDOW_NAME)
 * show image in a window
 */
PREDICATE(showimg_win, 2) {
    term_t t1 = A1.ref;
    term_t t2 = A2.ref;
    char *p1;
    char *p2;
    if (PL_get_atom_chars(t1, &p1)) {
        const string add(p1);
        Mat* img = str2ptr<Mat>(add);
        if (PL_get_atom_chars(t2, &p2)) {
            string window_name(p2);
            namedWindow(window_name);//, CV_WINDOW_NORMAL);
            Mat frame = img->clone();
            cvtColor(frame, frame, COLOR_Lab2BGR);
            imshow(window_name, frame);
            waitKey(0);
            destroyWindow(window_name);
            return TRUE;
        } else
            return LOAD_ERROR("showimg_win/2", 2,
                              "WINDOW_NAME", "STRING");
    } else
        return LOAD_ERROR("showimg_win/2", 1, "ADD", "STRING");
}

/* show2imgs_win(ADD1, ADD2, WINDOW_NAME)
 * show 2 images in a window
 */
PREDICATE(show2imgs_win, 3) {
    char *p1 = (char*) A1;
    char *p2 = (char*) A2;
    char *p3 = (char*) A3;

    const string window_name(p3);

    cv::Mat* im1 = str2ptr<Mat>(p1);
    cv::Mat* im2 = str2ptr<Mat>(p2);
    cv::Size sz1 = im1->size();
    cv::Size sz2 = im2->size();
    cv::Mat im3(sz1.height, sz1.width + 10 + sz2.width, CV_8UC3);
    // Move right boundary to the left.
    im3.adjustROI(0, 0, 0, -sz2.width - 10);
    im1->copyTo(im3);
    // Move the left boundary to the right, right boundary to the right.
    im3.adjustROI(0, 0, -sz1.width - 10, sz2.width + 10);
    im2->copyTo(im3);
    // restore original ROI.
    im3.adjustROI(0, 0, sz1.width + 10, 0);
    cvtColor(im3, im3, COLOR_Lab2BGR);
    imshow(window_name, im3);
    waitKey(0);
    destroyWindow(window_name);
    return TRUE;
}

/* showvid_win(ADD, WINDOW_NAME)
 * show a video in stack (ADD)
 */
PREDICATE(showvid_win, 2) {
    term_t t1 = A1.ref;
    term_t t2 = A2.ref;
    char *p1;
    char *p2;
    if (PL_get_atom_chars(t1, &p1)) {
        if (PL_get_atom_chars(t2, &p2)) {
            string add(p1);
            string window_name(p2);

            VideoCapture *vid = str2ptr<VideoCapture>(add);
            long frame_total = vid->get(CV_CAP_PROP_FRAME_COUNT);
            long frame_start = 0;
            long frame_end = frame_total - 1;
            double frame_rate = vid->get(CV_CAP_PROP_FPS);
            Mat frame;
            namedWindow(window_name);//, CV_WINDOW_NORMAL);
            int delay = 1000/frame_rate;
            bool stop = false;
            long frame_current = frame_start;
            while(!stop) {
                if(!vid->read(frame)) {
                    cerr << "Reading frame " << frame_current
                         << " failed" << endl;
                    return FALSE;
                }
                imshow(window_name, frame);
                int c = waitKey(delay);
                if((char) c == 27 || frame_current > frame_end)
                    stop = true;
                else if(c >= 0)
                    waitKey(0);
                ++frame_current;
            }
            destroyWindow(window_name);
            return TRUE;
        } else
            return LOAD_ERROR("showvid_win/2", 2, "WINDOW_NAME", "STRING");
    } else
        return LOAD_ERROR("showvid_win/2", 1, "ADD", "STRING");
}

/* showseq_win(ADD, WINDOW_NAME)
 * show a video in stack (ADD)
 */
PREDICATE(showseq_win, 2) {
    term_t t1 = A1.ref;
    term_t t2 = A2.ref;
    char *p1;
    char *p2;
    if (PL_get_atom_chars(t1, &p1)) {
        if (PL_get_atom_chars(t2, &p2)) {
            string add(p1);
            string window_name(p2);
            vector<Mat> *seq = str2ptr<vector<Mat>>(add);
            long frame_total = seq->size();
            long frame_start = 0;
            long frame_end = frame_total;
            double frame_rate = 24;
            Mat frame;
            namedWindow(window_name);//, CV_WINDOW_NORMAL);
            int delay = 1000/frame_rate;
            bool stop = false;
            long frame_current = frame_start;
            for (auto it = seq->begin(); it != seq->end() && !stop; ++it) {
                frame = (Mat) *it;
                Mat frame_copy = frame.clone();
                cvtColor(frame_copy, frame_copy, COLOR_Lab2BGR);
                imshow(window_name, frame_copy);
                int c = waitKey(delay);
                if((char) c == 27 || frame_current > frame_end)
                    stop = true;
                else if(c >= 0)
                    waitKey(0);
                ++frame_current;
            }
            destroyWindow(window_name);
            return TRUE;
        } else
            return LOAD_ERROR("showseq_win/2", 2, "WINDOW_NAME", "STRING");
    } else
        return LOAD_ERROR("showseq_win/2", 1, "ADD", "STRING");    
}

/* seq_img(SEQ, IDX, IMG)
 * get an image (IMG) from image sequence (SEQ), starting from 0
 */
PREDICATE(seq_img, 3) {
    term_t t1 = A1.ref;
    char *p1;
    if (PL_get_atom_chars(t1, &p1)) {
        string add(p1);
        vector<Mat> *seq = str2ptr<vector<Mat>>(add);
        
        term_t t2 = A2.ref;
        int p2;
        if (PL_get_integer(t2, &p2)) {
            if (p2 < 0 || p2 >= (int) seq->size())
                return LOAD_ERROR("seq_img/3", 2, "IDX",
                                  " 0 < NUMBER < size");
            Mat *img = element_ptr_in_vector<Mat>(seq, p2);
            string add2 = ptr2str(img);
            term_t t3 = PL_new_term_ref();
           
            if (PL_put_atom_chars(t3, add2.c_str())) {
                A3 = PlTerm(t3);
                // if no size_2d(A3, _, _) fact, assert it
                PlTermv av_size(3);
                av_size[0] = A3;
                PlQuery q("size_2d", av_size);
                if (q.next_solution())
                    return TRUE;
                int col = img->cols;
                int row = img->rows;
                PlTermv size_2d_args(3);
                size_2d_args[0] = A3;
                size_2d_args[1] = col;
                size_2d_args[2] = row;
                PlTermv size_2d_atom(1);
                size_2d_atom[0] = PlCompound("size_2d", size_2d_args);
                PlCall("assertz", size_2d_atom);
                return TRUE;
            } else
                return PUT_ERROR("seq_img/3", 3, "IMG", "STRING");
        } else
            return LOAD_ERROR("seq_img/3", 2, "IDX", "NUMBER");
    } else
        return LOAD_ERROR("seq_img/3", 1, "SEQ", "STRING");
}

/* close_window(WINDOW_NAME)
 * close a visualizing window
 */
PREDICATE(close_window, 1) {
    term_t t1 = A1.ref;
    char *p1;
    if (PL_get_atom_chars(t1, &p1)) {
        string window_name(p1);
        destroyWindow(window_name);
        return TRUE;
    } else
        return LOAD_ERROR("close_window/1", 1, "ADD", "STRING");
}

/* close_all_windows
 * close all windows.
 */
PREDICATE(close_all_windows, 0) {
    destroyAllWindows();
    return TRUE;
}

/* clone_img(IMG1, IMG2)
 * clone an image (mostly for drawing), REMEMBER TO RELEASE IT!
 */
PREDICATE(clone_img, 2) {
    char *p1;
    if (!(p1 = (char*) A1))
        return FALSE;
    const string add_img(p1); // address
    Mat *img = str2ptr<Mat>(add_img);
    Mat *newimg = new Mat(img->clone());
    // convert returning
    string add = ptr2str(newimg);
    A2 = PlTerm(add.c_str());
    // assert size_2d
    int col = img->cols;
    int row = img->rows;
    PlTermv size_2d_args(3);
    size_2d_args[0] = A2;
    size_2d_args[1] = col;
    size_2d_args[2] = row;
    PlTermv size_2d_atom(1);
    size_2d_atom[0] = PlCompound("size_2d", size_2d_args);
    PlCall("assertz", size_2d_atom);
    return TRUE;
}

/* clone_seq(SEQ1, SEQ2)
 * clone an image sequence (mostly for drawing), REMEMBER TO RELEASE IT!
 */
PREDICATE(clone_seq, 2) {
    char *p1;
    if (!(p1 = (char*) A1))
        return FALSE;
    const string add_seq(p1); // address
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);    
    // copy image sequence
    vector<Mat> *newseq = new vector<Mat>();
    for (auto it = seq->begin(); it != seq->end(); ++it) {
        newseq->push_back(((Mat) *it).clone());
    }
    string add = ptr2str(newseq);
    A2 = PlTerm(add.c_str());
    // assert size_2d and size_3d
    int col = (*seq)[0].cols;
    int row = (*seq)[0].rows;
    int dur = seq->size();
    PlTermv size_2d(3);
    PlTermv size_3d(4);
    size_2d[0] = A2;
    size_2d[1] = col;
    size_2d[2] = row;
    size_3d[0] = A2;
    size_3d[1] = col;
    size_3d[2] = row;
    size_3d[3] = dur;
    PlTermv size_2d_atom(1);
    size_2d_atom[0] = PlCompound("size_2d", size_2d);
    PlTermv size_3d_atom(1);
    size_3d_atom[0] = PlCompound("size_3d", size_3d);        
    PlCall("assertz", size_2d_atom);
    PlCall("assertz", size_3d_atom);
    return TRUE;
}

/* subimg_2d(Img1, [X, Y, RX, RY], Img2)
 * get a sub image (Img2) from Img1
 * @X,Y: center
 * @RX,RY: radius
 */
PREDICATE(subimg_2d, 3) {
    // source image
    char *p1 = (char*) A1;
    const string add_img(p1);
    Mat *img = str2ptr<Mat>(add_img);
    // ROI
    vector<int> vec = list2vec<int>(A2, 4);
    // get subimage
    Mat *newimg = cv_get_subimage(img, Scalar(vec[0], vec[1], vec[2], vec[3]));
    // convert returning
    string add = ptr2str(newimg);
    A3 = PlTerm(add.c_str());    
    // assert size_2d
    int col = newimg->cols;
    int row = newimg->rows;
    PlTermv size_2d_args(3);
    size_2d_args[0] = A3;
    size_2d_args[1] = col;
    size_2d_args[2] = row;
    PlTermv size_2d_atom(1);
    size_2d_atom[0] = PlCompound("size_2d", size_2d_args);
    PlCall("assertz", size_2d_atom);
    return TRUE;
}

/* resize_img_2d(Img1, [Width, Height], Img2)
 * get a sub image (Img2) from Img1
 * @Width, Height: size of the resized image
 */
PREDICATE(resize_img_2d, 3) {
    // source image
    char *p1 = (char*) A1;
    const string add_img(p1);
    Mat *img = str2ptr<Mat>(add_img);
    // size
    vector<int> vec = list2vec<int>(A2, 2);
    // get subimage
    Mat *newimg = cv_resize_image(img, vec[0], vec[1]);
    // convert returning
    string add = ptr2str(newimg);
    A3 = PlTerm(add.c_str());
    // assert size_2d
    int col = newimg->cols;
    int row = newimg->rows;
    PlTermv size_2d_args(3);
    size_2d_args[0] = A3;
    size_2d_args[1] = col;
    size_2d_args[2] = row;
    PlTermv size_2d_atom(1);
    size_2d_atom[0] = PlCompound("size_2d", size_2d_args);
    PlCall("assertz", size_2d_atom);
    return TRUE;
}

/* gaussian_blur(ImgIn, Param, ImgOut)
 * Gaussian blur of given image
 * @ImgIn/Out: input/output image
 * @Param = [SizeX, SizeY, SigmaX, SigmaY]: SizeX/Y - gaussian kernel size,
 *          SigmaX/Y - gaussian sigma in X/Y direction
 */
PREDICATE(gaussian_blur, 3) {
    // source image
    char *p1 = (char*) A1;
    const string add_img(p1);
    Mat *img = str2ptr<Mat>(add_img);
    // parameter
    vector<int> vec = list2vec<int>(A2, 4);
    Mat *newimg = new Mat();
    GaussianBlur(*img, *newimg, Size(vec[0], vec[1]), vec[2], vec[3]);
    // convert returning
    string add = ptr2str(newimg);
    A3 = PlTerm(add.c_str());
    // assert size_2d
    int col = newimg->cols;
    int row = newimg->rows;
    PlTermv size_2d_args(3);
    size_2d_args[0] = A3;
    size_2d_args[1] = col;
    size_2d_args[2] = row;
    PlTermv size_2d_atom(1);
    size_2d_atom[0] = PlCompound("size_2d", size_2d_args);
    PlCall("assertz", size_2d_atom);
    return TRUE;
}

/* create_superpixels(Img, Param, SP)
 *  create superpixels for image
 * @Img: input image
 * @SP: memory address of output superpixel
 * @Param = [Algorithm, RegionSize, Ruler, IterNum, Connect]: Parameters for LSC superpixel
 */
PREDICATE(create_superpixels, 3) {
    // source image
    char *p1 = (char*) A1;
    const string add_img(p1);
    Mat *img = str2ptr<Mat>(add_img);
    // parameter
    vector<double> vec = list2vec<double>(A2, 5);
    SuperPixels *sp = new SuperPixels(img, (int) vec[0], (int) vec[1], (float) vec[2], (int) vec[3], (int) vec[4]);
    string add_sp = ptr2str(sp);
    A3 = add_sp.c_str();
    // assert size_2d & num_superpixels
    int num_sp = sp->getNumberOfSuperpixels();    
    PlTermv num_sp_args(2);
    num_sp_args[0] = A3;
    num_sp_args[1] = num_sp;
    PlTermv num_sp_atom(1);
    num_sp_atom[0] = PlCompound("num_superpixels", num_sp_args);
    PlCall("assertz", num_sp_atom);
    
    int col = sp->getWidth();
    int row = sp->getHeight();
    PlTermv size_2d_args(3);
    size_2d_args[0] = A3;
    size_2d_args[1] = col;
    size_2d_args[2] = row;
    PlTermv size_2d_atom(1);
    size_2d_atom[0] = PlCompound("size_2d", size_2d_args);
    PlCall("assertz", size_2d_atom);
    return TRUE;
}

/* load_superpixels(Path, SP)
 *  load superpixels from file
 * @Path: path of input file
 * @SP: memory address of output superpixel
 */
PREDICATE(load_superpixels, 2) {
    // source image
    char *p1 = (char*) A1;
    const string file_path(p1);
    // load from file
    SuperPixels *sp = new SuperPixels(file_path);
    string add_sp = ptr2str(sp);
    A2 = add_sp.c_str();
    // assert size_2d
    int num_sp = sp->getNumberOfSuperpixels();
    PlTermv num_sp_args(2);
    num_sp_args[0] = A2;
    num_sp_args[1] = num_sp;
    PlTermv num_sp_atom(1);
    num_sp_atom[0] = PlCompound("num_superpixels", num_sp_args);
    PlCall("assertz", num_sp_atom);

    int col = sp->getWidth();
    int row = sp->getHeight();
    PlTermv size_2d_args(3);
    size_2d_args[0] = A2;
    size_2d_args[1] = col;
    size_2d_args[2] = row;
    PlTermv size_2d_atom(1);
    size_2d_atom[0] = PlCompound("size_2d", size_2d_args);
    PlCall("assertz", size_2d_atom);
    return TRUE;
}

/* show_superpixels(Img, SP)
 *  show superpixels for image
 * @Img: input image
 * @SP: memory address of output superpixel
 */
PREDICATE(show_superpixels, 2) {
    // source image
    char *p1 = (char*) A1;
    const string add_img(p1);    
    Mat *img = str2ptr<Mat>(add_img);
    // address of super pixels
    char *p2 = (char*) A2;
    const string add_sp(p2);
    SuperPixels *sp = str2ptr<SuperPixels>(add_sp);
    show_super_pixels(img, sp);
    return TRUE;
}

/* get_sp_pixels(+SP, +Label, -Points)
 *  get pixels in superpixel map
 * @SP: memory address of output superpixel
 * @Label: the id of superpixel
 * @Points: list of pixels
 */
PREDICATE(get_sp_pixels, 3) {
    // source image
    char *p1 = (char*) A1;
    const string add_sp(p1);
    SuperPixels *sp = str2ptr<SuperPixels>(add_sp);
    // address of super pixels
    int id = (int) A2;
    vector<vector<long>> pts = sp->getLabelPoints(id);
    return A3 = vecvec2list<long>(pts, -1, 2);
}

/* get_sps_pixels(+SP, +[Labels], -Points)
 *  get pixels in superpixel map
 * @SP: memory address of output superpixel
 * @[Labels]: the list of ids of superpixels
 * @Points: list of pixels
 */
PREDICATE(get_sps_pixels, 3) {
    // source image
    char *p1 = (char*) A1;
    const string add_sp(p1);
    SuperPixels *sp = str2ptr<SuperPixels>(add_sp);
    // address of super pixels
    vector<int> ids = list2vec<int>(A2);
    vector<vector<long>> pts = sp->getLabelsPoints(ids);
    return A3 = vecvec2list<long>(pts, -1, 2);
}

/* get_sp_all_adj_pairs(+SP, -[List-of-Pairs])
 *  get adjacent superpixel pairs
 * @SP: memory address of output superpixel
 * @[List-of-Pairs]: the list of adjacent superpixel pairs
 */
PREDICATE(get_sp_all_adj_pairs, 2) {
    // source image
    char *p1 = (char*) A1;
    const string add_sp(p1);
    SuperPixels *sp = str2ptr<SuperPixels>(add_sp);
    // call getAdjPairs
    vector<vector<long>> pairs = sp->getAdjPairs();
    return A2 = vecvec2list<long>(pairs, -1, 2);
}

/* get_sp_position(+SP, +Label, -Position)
 *  get adjacent superpixel pairs
 * @SP: memory address of output superpixel
 * @Label: ID of superpixel
 * @Position: [X, Y], position of superpixel center
 */
PREDICATE(get_sp_position, 3) {
    // source image
    char *p1 = (char*) A1;
    const string add_sp(p1);
    SuperPixels *sp = str2ptr<SuperPixels>(add_sp);
    // call getAdjPairs
    int id = (int) A2;
    vector<long> pos = sp->getLabelPosition(id);
    return A3 = vec2list<long>(pos, 2);
}

/* get_sp_pixels_labels(+SP, +Pts, -Labels)
 * @SP: memory address of output superpixel
 * @Pts: Points on image
 * @Labels: ID of superpixel
 */
PREDICATE(get_sp_pixels_labels, 3) {
    // source image
    char *p1 = (char*) A1;
    const string add_sp(p1);
    SuperPixels *sp = str2ptr<SuperPixels>(add_sp);
    // point list
    vector<Scalar> pts = point_list2vec(A2);
    // get labels
    const vector<long> labels = sp->getPointsLabels(pts);
    return A3 = vec2list<long>(labels);
}

/* save_superpixels(SP, FilePath)
 *  save superpixel map
 * @SP: memory address of output superpixel
 */
PREDICATE(save_superpixels, 2) {
    // source image
    char *p1 = (char*) A1;
    const string add_sp(p1);
    SuperPixels *sp = str2ptr<SuperPixels>(add_sp);
    char *p2 = (char*) A2;
    const string filepath(p2);
    sp->saveLabels(filepath);
    return TRUE;
}

/* release_sp(SP)
 *  release super pixel SLIC
 * @SP: memory address of output superpixel
 */
PREDICATE(release_sp, 1) {
    // source image
    char *p1 = (char*) A1;
    const string add_sp(p1);
    SuperPixels *sp = str2ptr<SuperPixels>(add_sp);
    PlTermv num_sp_args(2);
    num_sp_args[0] = A1;
    PlTermv num_sp_atom(1);
    num_sp_atom[0] = PlCompound("num_superpixels", num_sp_args);
    PlCall("retractall", num_sp_atom);
    // retract size_2d
    PlTermv size_2d_args(3);
    size_2d_args[0] = A1;
    PlTermv size_2d_atom(1);
    size_2d_atom[0] = PlCompound("size_2d", size_2d_args);
    PlCall("retractall", size_2d_atom);
    delete sp;
    return TRUE;
}
