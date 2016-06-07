/* Handels SWI-Prolog PL_* returns
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */
#ifndef _ERRORS_HPP
#define _ERRORS_HPP

#include <SWI-cpp.h>
#include <SWI-Prolog.h>

#include <iostream>
#include <string>

using namespace std;

int LOAD_ERROR(string pred_name, int arg_num, string arg_name, string type) {
    cerr << "[" << pred_name << "] Reading argument #" << arg_num
         << " " << arg_name << "(" << type << ") error!" << endl;
    return FALSE;
}

int PUT_ERROR(string pred_name, int arg_num, string arg_name, string type) {
    cerr << "[" << pred_name << "] Writing argument #" << arg_num
         << " " << arg_name << "(" << type << ") error!" << endl;
    return FALSE;
}

#endif
