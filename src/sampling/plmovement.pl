/* Prolog sampling
 *     Sample *continuous movements* on video (*image sequence*)
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

/* movement(+Ellipse, +Frame, -Direction)
 */
movement(Elps, Frm, Dir):-
    Elps = [[_, _, Frm0], _], % current frame
    
