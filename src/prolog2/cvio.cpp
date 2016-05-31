/* IO library for prolog
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

#include "../img2/io.hpp"
#include "../utils2/memread.hpp"
#include "../utils2/errors.hpp"
#include "../utils2/utils.hpp"

#include <opencv2/core/core.hpp>
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
        string add = ptr2str(img); // address of image in stack
        term_t t2 = PL_new_term_ref();
        if (PL_put_atom_chars(t2, add.c_str())) {
            A2 = PlTerm(t2);
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
        } else
            return PUT_ERROR("load_img/2", 2, "ADD", "STRING");
    } else
        return LOAD_ERROR("load_img/2", 1, "PATH", "STRING");
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
        }
        else
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
        string add(p1);
        Mat* img = str2ptr<Mat>(p1);
        if (PL_get_atom_chars(t2, &p2)) {
            string window_name(p2);
            namedWindow(window_name, WINDOW_AUTOSIZE);
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
            long frame_end = frame_total;
            double frame_rate = vid->get(CV_CAP_PROP_FPS);
            Mat frame;
            namedWindow(window_name);
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
            namedWindow(window_name);
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
 * get an image (IMG) from image sequence (SEQ)
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
                return LOAD_ERROR("seq_img/3", 2, "IDX", " 0 < NUMBER < size");
            Mat *img = element_ptr_in_vector<Mat>(seq, p2);
            string add2 = ptr2str(img);
            term_t t3 = PL_new_term_ref();
            if (PL_put_atom_chars(t3, add2.c_str())) {
                return A3 = PlTerm(t3);
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
    char *p1 = (char*) A1;
    const string add_img(p1); // address
    Mat *img = str2ptr<Mat>(add_img);
    Mat *newimg = new Mat(img->clone());
    // convert returning
    string add = ptr2str(newimg);
    return A2 = PlTerm(add.c_str());    
}

/* clone_seq(SEQ1, SEQ2)
 * clone an image sequence (mostly for drawing), REMEMBER TO RELEASE IT!
 */
PREDICATE(clone_seq, 2) {
    char *p1 = (char*) A1;
    const string add_seq(p1); // address
    vector<Mat> *seq = str2ptr<vector<Mat>>(add_seq);    
    // copy image sequence
    vector<Mat> *newseq = new vector<Mat>();
    for (auto it = seq->begin(); it != seq->end(); ++it) {
        newseq->push_back(((Mat) *it).clone());
    }
    string add = ptr2str(newseq);
    return A2 = PlTerm(add.c_str());    
}
