#!/usr/bin/python3

import os
import glob
import random
import shutil

if __name__ == "__main__":
    path = input("Directory path: ")
    dirs = os.listdir(path)

    sizes = [1, 2, 4, 8, 16, 32, 64, 128]
    
    for i in list(range(1, 6)):
        f11_str = path + "/1[1-2]*.jpg"
        f12_str = path + "/01*.jpg"
        f2_str = path + "/0[2-4]*.jpg"
        f3_str = path + "/0[5-7]*.jpg"
        f41_str = path + "/0[8-9]*.jpg"
        f42_str = path + "/10*.jpg"
        pics1 = glob.glob(f11_str)
        pics1.extend(glob.glob(f12_str))
        pics2 = glob.glob(f2_str)
        pics3 = glob.glob(f3_str)
        pics4 = glob.glob(f41_str)
        pics4.extend(glob.glob(f42_str))
        d_train1 = random.sample(pics1, 32)
        d_test = list(set(pics1) - set(d_train1))
        d_train2 = random.sample(pics2, 32)
        d_test.extend(set(pics2) - set(d_train2))
        d_train3 = random.sample(pics3, 32)
        d_test.extend(set(pics3) - set(d_train3))
        d_train4 = random.sample(pics4, 32)
        d_test.extend(set(pics4) - set(d_train4))

        fexp = open(path + '/exp_' + str(i) + '.txt', 'w')
        for s in sizes:
            if (s >= 4):
                d_train = random.sample(d_train1, s/4)
                d_train.extend(random.sample(d_train2, s/4))
                d_train.extend(random.sample(d_train3, s/4))
                d_train.extend(random.sample(d_train4, s/4))
            elif (s == 2):
                dd = [d_train1, d_train2, d_train3, d_train4]
                dd_train = random.sample(dd, 2)
                d_train = random.sample(dd_train[0], 1)
                d_train.extend(random.sample(dd_train[1], 1))
            else:
                d_train = d_train1
                d_train.extend(d_train2)
                d_train.extend(d_train3)
                d_train.extend(d_train4)
                d_train = random.sample(d_train, s)
            l = 'train #' + str(s) + '\t['
            for f in d_train:
                f_str = '"' + f.split('/')[-1] + '"'
                l = l + f_str + ','
            l = l[:-1] + ']\n'
            fexp.write(l)
            fexp.write('\n')
            #print(d_train)
        l = 'test\t['
        for f in d_test:
            f_str = '"' + f.split('/')[-1] + '"'
            l = l + f_str + ','
        l = l[:-1] + ']\n'
        fexp.write(l)
        fexp.close()
    
