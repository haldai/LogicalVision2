# LogicalVision2 #

Symbolic computer vision tool with SWI-Prolog and OpenCV.

## Requirement ##

1. SWI-Prolog (>=7.0) compiled with `EXTRA_PKGS=clib` and multi-threading enabled.

2. OpenCV (>=3.0).

3. Armadillo library for matrix handling.

4. [Optional] Qt5 if you want a moderner OpenCV displaying, complile your OpenCV with Qt option on.

## Compile ##

`cd src/ && make -j$nproc`

## Usage ##

After compilation you will get `cvio.so`, `cvdraw.so`, `cvsampler.so` `cvstats.so` in `libs` folder. Simply load them in SWI-Prolog with:

```prolog
load_foreign_library(foreign('libs/cvio.so')).
load_foreign_library(foreign('libs/cvsampler.so')).
load_foreign_library(foreign('libs/cvdraw.so')).
load_foreign_library(foreign('libs/cvstats.so')).
```

Please see the source codes for detail predicates.

### Example ###

The first example is about video I/O:

```prolog
?- load_foreign_library(foreign('libs/cvio.so')).
?- load_video('../../data/Protist.mp4', A), showvid_win(A, debug).
```

During video playing, press `ESC` to quit, any other key to pause.

The second example learns ambiguity from a crater image:

```prolog
cd src/learning
swipl ambiguity.pl
?- a.
```

If the code is built, you should firstly see an image of the crater with 4 abduced theories.

The third example learns the background knowledge of lighting w.r.t. convexity for ambiguity abduction:

```prolog
cd src/learning
swipl light.pl
?- a('01001').
```

The output should be a learned logic program of lighting, however the learning is based on random samplings of low-level features, so sometime the output would be nothing. In this case, please try again.

## Further details ##

Wang-Zhou Dai, Stephen H. Muggleton, and Zhi-Hua Zhou. Logical Vision: Meta-interpretive learning for simple geometrical concepts. In _Late Breaking Paper Proceedings of the 25th International Conference on Inductive Logic Programming_, pages 1–16. CEUR, 2015.

Wang-Zhou Dai and Zhi-Hua Zhou. Combining logic abduction and statistical induction: Discovering written primitives with human knowledge. In _Proceedings of the 31st AAAI Conference on Artificial Intelligence (AAAI'17)_, San Francisco, CA, 2017.


## Contact ##

Wang-Zhou Dai

LAMDA Group, Nanjing University

[daiwz@lamda.nju.edu.cn](mailto:daiwz@lamda.nju.edu.cn)

[http://lamda.nju.edu.cn/daiwz](http://lamda.nju.edu.cn/daiwz)

## License ##

The code is protected by GPLv3.
