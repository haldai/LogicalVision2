% background knowledge for abducing objects

% An object is defined like this:
% 1. Background is an object;
% 2. An object must connect with other objects: adjacent, include or be included.
% 3. Two connected (adjacent/included) objects are separated by a border line;
%    at least one side of the borderline belongs to one of the objects.
%    (Each point on the border line has a local maximum Scharr gradient)
% 4. An object is marked by either a rectangle or an ellipse with parameters:
%    e.g. rect([left_up_most_position], [x_length, y_length]);
%         elps([center], [a_axis, b_axis, tilt_angle]).



% assumption: background is a pure colored board, its boudary is
%             the canvas boundary.
obj(bk).
boundary(bk, rect([0, 0], [640, 360])).

% point inside of object
inside_of([PX, PY], rect([X, Y], [LX, LY])):-
    PX >= X, PX =< X + LX - 1,
    PY >= Y, PY =< Y + LY - 1.
inside_of([PX, PY], elps([CX, CY], [A, B, ALPHA])):-
    fail.

% abducing objects from line sampling results (edge_points)
% The sampled results is [edge_point_1, edge_point_2, ...], according to our
% assumption, each line segment [edge_point_n, edge_point_n+1] belongs
% to an object. This predicate abduces whether each edge_point is a noise
% point or an inside point

%obj([EP1 | EPs], )

