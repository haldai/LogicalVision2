/* http://blog.csdn.net/wangyaninglm/article/details/39182711
*/

#include <iostream> // for standard I/O  
#include <string>   // for strings  
#include <iomanip>  // for controlling float print precision   
#include <sstream>  // string to number conversion   
  
#include <opencv2/imgproc/imgproc.hpp>  // Gaussian Blur  
#include <opencv2/core/core.hpp>        // Basic OpenCV structures (cv::Mat, Scalar)  
#include <opencv2/highgui/highgui.hpp>  // OpenCV window I/O  
  
using namespace std;  
using namespace cv;  
  
// images  
Mat inputImg, showImg, segMask, segShowImg;  
  
// mask  
Mat fgScribbleMask, bgScribbleMask;  
  
  
// user clicked mouse buttons flags  
bool rButtonDown = false;  
bool lButtonDown = false;  
int scribbleRadius = 5;  
  
  
// mouse listener  
static void onMouse( int event, int x, int y, int, void* )  
{  
    //cout << "On Mouse: (" << x << "," << y << ")" <<endl;  
  
  
    if (event == CV_EVENT_LBUTTONDOWN)  
    {  
        lButtonDown = true;  
  
    }  
    else if (event == CV_EVENT_RBUTTONDOWN)  
    {  
        rButtonDown = true;  
  
    }  
    else if (event == CV_EVENT_LBUTTONUP)  
    {  
        lButtonDown = false;  
    }  
    else if (event == CV_EVENT_RBUTTONUP)  
    {  
        rButtonDown = false;  
    }  
    else if (event == CV_EVENT_MOUSEMOVE)  
    {  
        if (rButtonDown)  
        {  
            // scribble the background  
  
            circle(bgScribbleMask,Point(x,y),scribbleRadius, 255,-1);  
            circle(showImg,Point(x,y),scribbleRadius, CV_RGB(0,0,255),-1);  
  
        }  
        else if (lButtonDown)  
        {  
            // scribble the foreground  
  
            circle(fgScribbleMask,Point(x,y),scribbleRadius, 255,-1);  
            circle(showImg,Point(x,y),scribbleRadius, CV_RGB(255,0,0),-1);  
  
            //fgScribbleMask.at<char>(y,x)=(char)255;  
            // set variables using mask  
            //showImg.setTo(redColorElement,fgScribbleMask);  
  
            //showImg.at<Vec3b>(y,x)[0] = 0;  
            //showImg.at<Vec3b>(y,x)[1] = 0;  
            //showImg.at<Vec3b>(y,x)[2] = 255;  
        }  
  
    }  
  
  
    imshow("Scribble Image", showImg);  
    imshow("fg mask", fgScribbleMask);  
    imshow("bg mask", bgScribbleMask);  
}  
  
  
// clear everything before closing  
void destroyAll()  
{  
    // destroy all windows  
    destroyWindow("Input Image");  
    destroyWindow("Scribble Image");  

    destroyWindow("bg mask");  
    destroyWindow("fg mask");  
    destroyWindow("Segmentation Mask");  
    destroyWindow("Segmentation Image");  
  
    // clear all data  
    fgScribbleMask.release();  
    bgScribbleMask.release();  
    inputImg.release();  
    showImg.release();  
      
    segMask.release();  
    segShowImg.release();  
  
}  
  
// init all images/vars  
int init(char * imgFileName)  
{  
    // Read the file  
    inputImg = imread(imgFileName, CV_LOAD_IMAGE_COLOR);     
    showImg = inputImg.clone();  
    segShowImg = inputImg.clone();  
  
    // Check for invalid input  
    if(!inputImg.data )                                
    {  
        cout <<  "Could not open or find the image: " << imgFileName << std::endl ;  
        return -1;  
    }  
  
    // this is the mask to keep the user scribbles  
    fgScribbleMask.create(2,inputImg.size,CV_8UC1);  
    fgScribbleMask = 0;  
    bgScribbleMask.create(2,inputImg.size,CV_8UC1);  
    bgScribbleMask = 0;  
    segMask.create(2,inputImg.size,CV_8UC1);  
    segMask = 0;  
      
  
    // Create a window for display.  
    namedWindow( "Input Image", CV_WINDOW_AUTOSIZE );  
    namedWindow( "Scribble Image", CV_WINDOW_AUTOSIZE);  
  
    namedWindow( "fg mask", CV_WINDOW_AUTOSIZE );  
    namedWindow( "bg mask", CV_WINDOW_AUTOSIZE );  
  
    // Show our image inside it.  
    imshow( "Input Image", inputImg );                          
    imshow( "Scribble Image", showImg );    
  
    imshow("fg mask", fgScribbleMask);  
    imshow("bg mask", bgScribbleMask);  
  
    moveWindow("Scribble Image", 1,1);  
    moveWindow("Input Image", inputImg.cols + 50,1);  
    moveWindow("Bin Per Pixel", 2*(inputImg.cols + 50),1);  
    moveWindow("Edges", 2*(inputImg.cols + 55),1);  

    // set the callback on mouse  
    setMouseCallback("Scribble Image", onMouse, 0);  
    return 0;  
}  
  
  
int main(int argc, char *argv[])  
{  
    string image_name,numBinsStr,bhaSlopeStr;  
    cout << "input Parameters:" << endl;  
    cout << "image name: ";  
    cin >> image_name;  
      
  
    // get img name parameter  
    char * imgFileName = (char *)image_name.c_str();  
  
  
    if (init(imgFileName)==-1)  
    {  
        cout <<  "Could not initialize" << endl ;  
        return -1;  
    }  
  
  
  
    // Wait for a keystroke in the window  
    for (;;)  
    {  
        char key = waitKey(0);                            
        switch (key)  
        {  
        case 'q':  
            cout << "goodbye" << endl;  
            destroyAll();  
            return 0;  
        case '-':  
            //缩小画笔直径  
            if (scribbleRadius > 2)  
                scribbleRadius --;  
            cout << "current radius is " << scribbleRadius << endl;  
            break;  
        case '+':  
            if (scribbleRadius < 100)  
                scribbleRadius ++;  
            cout << "current radius is " << scribbleRadius << endl;  
            break;  
        case 's':  
            {  
                // this is where we store the results  
                segMask = 0;  
                inputImg.copyTo(segShowImg);  
                //inputImg.copyTo(showImg);  
  
                imwrite("bg.bmp",bgScribbleMask);
                imwrite("fg.bmp",fgScribbleMask);  
                break;  
  
            }  
        case 'r':  
            {  
                cout << "resetting" << endl;  
                destroyAll();  
                if (init(imgFileName)==-1)  
                {  
                    cout <<  "could not initialize" << std::endl ;  
                    return -1;  
                }  
                break;  
            }  
        }  
    }  
    return 0;  
}  
