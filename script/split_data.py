#!/usr/bin/python3

import os
import glob
import random
import shutil

if __name__ == "__main__":
    path = input("Directory path: ")
    dirs = os.listdir(path)

    train = []
    test = []
    for d in dirs:
        f_str = path + "/" + d + "/*.jpg"
        pics = glob.glob(f_str)
        num_train = 32
        num_test = 58
        d_train = random.sample(pics, num_train)
        train.extend(d_train)
        d_test = set(pics) - set(d_train)
        test.extend(d_test)
    train_path = path + "/train"
    test_path = path + "/test"
    if not os.path.exists(train_path):
        os.makedirs(train_path)
    if not os.path.exists(test_path):
        os.makedirs(test_path)
    for jpg in train:
        shutil.copy2(jpg, train_path)
    for jpg in test:
        shutil.copy2(jpg, test_path)
    print("Finished.")
