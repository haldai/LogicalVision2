import cv2
import imutils
import argparse
import numpy as np

def getShape(contour):
    p = cv2.arcLength(contour, True)
    aV = cv2.approxPolyDP(contour, 0.04 * p, True)
    if len(aV) == 3:
        return 'Triangle'
    elif len(aV) == 4:
        (x, y, w, h) = cv2.boundingRect(aV)
        aspectRatio = w / float(h)
        if aspectRatio >= 0.95 and aspectRatio <= 1.95:
            return 'Square'
        else:
            return 'Rectangle'
    return 'Unknown'


ap = argparse.ArgumentParser()
ap.add_argument("-i", "--image", required=True,	help="Input Image")
args = vars(ap.parse_args())


inImage = cv2.imread(args['image']) #'tr.png')#('shapes_and_colors.png')
imgray = cv2.cvtColor(inImage,cv2.COLOR_BGR2GRAY)
ret,thresh = cv2.threshold(imgray,127,255,0)
im2, contours, hierarchy = cv2.findContours(thresh,cv2.RETR_EXTERNAL,cv2.CHAIN_APPROX_SIMPLE)

print len(contours)
for cont in contours:
    #print cv2.moments(cont)
    cv2.drawContours(inImage, [cont], -1, (0, 255, 0), 2)
    cM = cv2.moments(cont)
    print cM['m00']
    X = int(cM['m10'] /(cM['m00']+1))
    Y = int(cM['m01'] /(cM['m00']+1))
    cv2.putText(inImage, getShape(cont), (X, Y), cv2.FONT_HERSHEY_SIMPLEX,
		0.5, (255, 0, 0), 2)
    cv2.imshow("Image", inImage)

cv2.waitKey(0)
