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
