/'==============================================================================
Although the de?nitions of dual cones and polar cones can apply to any
set of vectors, they are of the most interest in the case in which the underlying
set of vectors is a cone in the nonnegative orthant of a Cartesian coordinate
system on IRn (the set of n-vectors all of whose elements are nonnegative).
In that case, the dual cone is just the full nonnegative orthant, and the polar
cone is just the nonpositive orthant (the set of all vectors all of whose elements
are nonpositive).
==============================================================================='/
#ifdef map_file
#macro irn_map ( polar, dual_cone, apply )
declare sub IRn ( )

' easy_matx.bi


#inclib "easy_matx"

'  For some reason, may need to duplicate types, but not constructors,
' in this *.bi

type Matrix
    dim as double m( any , any )
    declare constructor ( )
    declare constructor ( byval x as uinteger , byval y as uinteger )
end type
'
type Vector
dim as double v( any )
    declare constructor ( )
    declare constructor ( byval x as uinteger )
end type

'
'
' ---------------------------- operators -------------------------------
'
declare operator * ( byref a as Matrix , byref b as Matrix ) as Matrix
declare operator * ( byref a as Matrix , byref b as Vector ) as Vector

declare operator - ( byref a as Matrix , byref b as Matrix ) as Matrix
declare operator + ( byref a as Matrix , byref b as Matrix ) as Matrix
declare operator / ( byref a as Matrix , byref b as Matrix ) as Matrix

'   The rest from luxan, sciwiseg@gmail.com
'
'  Probably need to make this content
'  available under GNU Free Documentation License 1.2 
'
'
' ............................. procedures .............................
'
declare sub gen_z(z() as Matrix, idx as Integer)
declare sub gen_v(u() as Vector, idx as Integer)

declare sub prtv_v(u() as Vector, idx as Integer)

declare sub prt_z(z() as Matrix, idx as Integer)
declare sub prt_m(z as Matrix)
declare sub Vect_x_Matrix(M1 as Matrix, V1 as Vector, V3 as Vector)

declare sub prt_v(z as Vector)
'
end
#endmacro
#endif
