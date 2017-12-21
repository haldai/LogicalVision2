# swipl: load_foreign_library(foreign('cvio.so')).

SRCDIR = .
LIBSDIR = ../../libs
INCLUDEDIR = ../../include
SRCFILES = $(shell find $(SRCDIR) -name "*.cpp")
OBJFILES = $(patsubst %.cpp,%.o,$(SRCFILES))

CXX = g++
MAKE = make
CXXFLAGS = -Wall -std=c++11 -fPIC $(INCLUDE) $(COFLAGS)
COFLAGS = -gdwarf-2 -g3 -O0
LDFLAGS = -Wall -fPIC $(LIBS) $(COFLAGS)
INCLUDE = -I$(SRCDIR) -I$(INCLUDEDIR)
LIBS = -lm -L$(SRCDIR) -L$(LIBSDIR)

# Library prefix
MLPACK_HOME = /home/daiwz/.local

# SWI-prolog
CXXFLAGS_SWI = `pkg-config --cflags swipl`
LDFLAGS_SWI = `pkg-config --libs swipl`

# Opencv
CXXFLAGS_CV = `pkg-config --cflags opencv`
LDFLAGS_CV = `pkg-config --libs opencv`

# Armadillo
LDFLAGS_ARMA = -larmadillo

# Qt5
CXXFLAGS_QT = `pkg-config --cflags Qt5Gui`
LDFLAGS_QT = `pkg-config --libs Qt5Gui`

# mlpack library
CXXFLAGS_MLPACK = -I$(MLPACK_HOME)/include
LDFLAGS_MLPACK = -L$(MLPACK_HOME) -L$(LDFLAGS_ARMA) -lmlpack -lboost_serialization

#all:cvcluster.so
all:cvio.so cvsampler.so cvdraw.so cvcluster.so cvstats.so cvknn.so

cvio.so:cvio.o
	$(CXX) -shared $(LIBS) cvio.o -o cvio.so $(LDFLAGS_CV) $(LDFLAGS_SWI) $(LDFLAGS_QT) $(LDFLAGS_MLPACK)
	mv cvio.so $(LIBSDIR)/

cvio.o:
	$(CXX) $(CXXFLAGS) $(CXXFLAGS_CV) $(CXXFLAGS_SWI) $(CXXFLAGS_QT) -c cvio.cpp

cvsampler.so:cvsampler.o
	$(CXX) -shared $(LIBS) cvsampler.o -o cvsampler.so $(LDFLAGS_CV) $(LDFLAGS_SWI) $(LDFLAGS_QT) $(LDFLAGS_ARMA) $(LDFLAGS_MLPACK)
	mv cvsampler.so $(LIBSDIR)/

cvsampler.o:
	$(CXX) $(CXXFLAGS) $(CXXFLAGS_CV) $(CXXFLAGS_SWI) $(CXXFLAGS_QT) -c cvsampler.cpp

cvdraw.so:cvdraw.o
	$(CXX) -shared $(LIBS) cvdraw.o -o cvdraw.so $(LDFLAGS_CV) $(LDFLAGS_SWI) $(LDFLAGS_QT) $(LDFLAGS_MLPACK)
	mv cvdraw.so $(LIBSDIR)/

cvdraw.o:
	$(CXX) $(CXXFLAGS) $(CXXFLAGS_CV) $(CXXFLAGS_SWI) $(CXXFLAGS_QT) -c cvdraw.cpp

cvcluster.so:cvcluster.o
	$(CXX) -shared $(LIBS) cvcluster.o -o cvcluster.so $(LDFLAGS_MLPACK) $(LDFLAGS_CV)
	mv cvcluster.so $(LIBSDIR)/

cvcluster.o:
	$(CXX) $(CXXFLAGS) $(CXXFLAGS_CV) $(CXXFLAGS_SWI) $(CXXFLAGS_QT) -c cvcluster.cpp

cvstats.so:cvstats.o svm.o
	$(CXX) -shared $(LIBS) cvstats.o svm.o -o cvstats.so $(LDFLAGS_MLPACK) $(LDFLAGS_CV)
	mv cvstats.so $(LIBSDIR)/

cvstats.o:
	$(CXX) $(CXXFLAGS) $(CXXFLAGS_CV) $(CXXFLAGS_SWI) $(CXXFLAGS_QT) -c cvstats.cpp

svm.o:
	$(CXX) $(CXXFLAGS) -c svm.cpp

cvknn.so:cvknn.o
	$(CXX) -shared $(LIBS) cvknn.o -o cvknn.so $(LDFLAGS_MLPACK) $(LDFLAGS_CV)
	mv cvknn.so $(LIBSDIR)/

cvknn.o:
	$(CXX) $(CXXFLAGS) $(CXXFLAGS_CV) $(CXXFLAGS_SWI) $(CXXFLAGS_QT) -c cvknn.cpp



clean:
	rm -rf *.o *.so
	rm ../../libs/cv*.so

