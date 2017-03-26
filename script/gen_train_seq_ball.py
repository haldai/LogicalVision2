#!/usr/bin/python3

import os
import glob
import random
import shutil
import copy

if __name__ == "__main__":
    #path = input("Directory path: ")
    path = "../data/MobileRobotAndBall1"
    dirs = os.listdir(path)

    sizes = [1, 2, 4, 8, 16, 32, 64, 128]
    train_size = 128
    
    for i in list(range(1, 6)):
        f_str = path + "/raw_images/*.jpg"
        print(f_str)
        pics = glob.glob(f_str)
        d_train = copy.deepcopy(random.sample(pics, train_size))
        d_test = copy.deepcopy(list(set(pics) - set(d_train)))

        fexp = open(path + '/trial_' + str(i) + '.pl', 'w')
        for s in sizes:
            print(s)
            d_train1 = copy.deepcopy(d_train)
            d_train2 = random.sample(d_train1, s)
            l = 'ball_train_' + str(s) + '(['
            for f in d_train2:
                f_str = f.split('/')[-1].split('.')[0]
                l = l + f_str + ','
            l = l[:-1] + ']).\n'
            fexp.write(l)
            fexp.write('\n')
            print(l)
        l = 'ball_test(['
        for f in d_test:
            f_str = f.split('/')[-1].split('.')[0]
            l = l + f_str + ','
        l = l[:-1] + ']).\n'
        print(l)
        fexp.write(l)
        fexp.close()
    
