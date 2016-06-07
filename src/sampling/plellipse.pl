/* Prolog sampling
 *     Sample ellipses on *image*.
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

/* Sample one ellipse in image
 * @Img: input image
 * @Elps: parameter of the sampled ellipse, Elps = [Center, [A, B, ALPHA]],
 *     Center is the center of the ellipse
 *     A, B are axis length
 *     ALPHA: tilt angle
 */
sample_ellipse(Img, Elps):-
    % TODO:
    Elps = [Center, [A, B, ALPHA]],
    fail.

