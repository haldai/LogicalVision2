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
along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
************************************************************************/
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
