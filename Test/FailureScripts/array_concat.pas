var ai : array of Integer;
var af : array of Float;
var ab : array of Boolean;

ai := ai + [ 1.5 ];
ai := [ 1.5 ] + ai;
ai := [ 1 ] + ai;
ai := ai + [ 1 ];
ai := ai + af;
ai := af + ai;

af := af + [ 1.5 ];
af := [ 1.5 ] + af;
af := [ 1 ] + af;
af := af + [ 1 ];
af := ai + af;
af := af + ai;

ab := ab + [true];
ab := ab + [1];
ab := ai + af;
ab := [false] + [true];