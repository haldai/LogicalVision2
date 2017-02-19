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
/* translate stack address to string for swi-prolog
 * ================================================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */
#ifndef _MEMREAD_H
#define _MEMREAD_H

#include <cstring>
#include <sstream>
#include <iostream>

typedef unsigned char unit8_t;
typedef unsigned short unit16_t;
typedef unsigned int unit32_t;
typedef unsigned long int unit64_t;
typedef void *memAdd;

using namespace std;

// read a memory address (pointer) from string
template <class Type>
Type *str2ptr(string addr) {
    stringstream ss;
    memAdd uAddr;

    ss << addr;
    ss >> uAddr;
    
    //cout << "Read address:\t" << uAddr << endl;

    Type *p = (Type *) uAddr;
    return p;
}

// translate memory address into string
string ptr2str(memAdd pointer) {
    ostringstream oss;
    oss << pointer << ends;
    //cout << "Write address:\t" << oss.str().c_str() << endl;
    return (string) oss.str();
}

#endif
