import cv2
import imutils
import argparse
import numpy as np

def getShape(contour):
    p = cv2.arcLength(contour, True)
    aV = cv2.approxPolyDP(contour, 0.04 * p, True)

    vertices = len(aV)
    if vertices == 3:
        return 'Triangle'
    elif vertices == 4:
        rect = cv2.minAreaRect(contour)
        contourArea = cv2.contourArea(contour)
        fittedArea = rect[1][0] * rect[1][1]
        #print "Countor Area:", contourArea , " Fiited A:", fittedArea
        (x, y, w, h) = cv2.boundingRect(aV)
        ar = w / float(h)
        if .95 * fittedArea <= contourArea and ar >= 0.95 and ar <= 1.05:
            return 'Square'
        else:
            return 'Rectangle'
    elif vertices == 5:
        return 'Pentagon'
    elif vertices == 6:
        return 'Hexagon'
    elif vertices == 7:
        return 'Heptagon'
    else:
        (xC, yC), radius  = cv2.minEnclosingCircle(contour)
        contourArea = cv2.contourArea(contour)
        fittedArea = radius*radius*3.14
        # print "Countor Area:", contourArea , " Circle A:", fittedArea
        if abs(contourArea-fittedArea) / max(contourArea, fittedArea) < 0.10:
            return 'Circle'
        else:
            return str(str(len(aV))+'-Polygon')
    return 'Unknown'


ap = argparse.ArgumentParser()
ap.add_argument("-i", "--image", required=True,	help="Input Image")
args = vars(ap.parse_args())

try:
    inImage = cv2.imread(args['image'])
    imgray = cv2.cvtColor(inImage,cv2.COLOR_BGR2GRAY)
    # Note that Logical-Vision has image dataset with BlackImage and White Background
    # So, Let's invert the image and then Detect Polygon
    # If you are using normal image. Make sure you comment following line
    #imgray = cv2.bitwise_not(imgray)
    ret,thresh = cv2.threshold(imgray,127,255,0)
    im2, contours, hierarchy = cv2.findContours(thresh,cv2.RETR_EXTERNAL,cv2.CHAIN_APPROX_SIMPLE)

    for cont in contours:
        cv2.drawContours(inImage, [cont], -1, (0, 255, 0), 2)
        cM = cv2.moments(cont)
        print cM['m00']
        X = int(cM['m10'] /(cM['m00']+1))
        Y = int(cM['m01'] /(cM['m00']+1))
        cv2.putText(inImage, getShape(cont), (X, Y), cv2.FONT_HERSHEY_SIMPLEX,
    		0.5, (255, 255, 255), 2)
        cv2.imshow("Image", inImage)
    cv2.waitKey(0)
except:
    print '!!! Could\'t find Image file. Make sure you provide correct Path and Filename-SPECIALLY EXTENSIONS !!!'
