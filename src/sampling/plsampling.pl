/* Sampling module
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */
:- load_foreign_library(foreign('../../libs/cvsampler.so')),
   load_foreign_library(foreign('../../libs/cvcluster.so')).

:- ensure_loaded(['../utils/utils.pl']),
   ensure_loaded(['../sampling/plregion.pl']),
   ensure_loaded(['../sampling/plsegment.pl']),
   ensure_loaded(['../sampling/plline.pl']).

