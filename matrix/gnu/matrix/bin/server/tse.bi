/'==============================================================================
See Fig. 2.2.8.2.
We generally drop the reference to an underlying set of vectors, and just
denote the dual cone of the set C as C 0 .
From the de?nition, we note that if S1 and S2 are sets of vectors in the
same vector space such that S1 ? S2 then C 0 (S1 ) ? C 0 (S2 ), or C10 ? C20 .
The polar cone and the dual cone of a double cone are clearly the same.
==============================================================================='/
#ifdef vector_spaces
#macro spaces ( S1, S2, C10, C20, C0 )
declare sub VectorSpaces ( )
'
'  matx_lrg.bas
'
'   Successive matrix vector multiplications .
'

'' compile with: fbc matx_lrg.bas




#cmdline "-exx"

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
declare operator - ( byref a as Vector , byref b as Vector ) as Vector

declare operator + ( byref a as Matrix , byref b as Matrix ) as Matrix
declare operator + ( byref a as Vector , byref b as Vector ) as Vector

declare operator / ( byref a as Matrix , byref b as Matrix ) as Matrix
declare operator / ( byref a as Vector , byref b as Vector ) as Vector

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
' ----------------------------------------------------------------------
'
dim as integer m_seq(0 to 3)={8,6,3,5}
dim as integer lms,nx,ny,i
lms=ubound(m_seq,1)
print(lms)
'
'    Matrix of matrices
'
dim as Matrix z(1 to lms)
'
'   Vector of vectors
'
dim as Vector u(0 to lms)
'
'   Dimension matrices and vectors .
'
for i=0 to lms-1
   nx=m_seq(i)
   ny=m_seq(i+1)
   z(i+1) = Matrix(ny , nx)
   u(i) = Vector(nx) ' subtract bias vector, apply threshold function; return ?
next i
'
'  ............... Assign values to input vector & matrices ............
'
gen_v(u(), 0)
print
print " Input vector, a matrix in another version ? "
print
prtv_v(u(), 0)
'
print   " matrix of matrices values "
print
'
for i=1 to lms
    gen_z(z() , i)
    prt_z(z() , i)
next i
print
'
print " sucessive vectors , matrices & vector x matrix calculations"
print " u(i) = z(i) * u(i - 1) "
print
'
print " u(0) "
prtv_v(u(), 0)
for i=1 to lms 
   print " z(";i;") "
   prt_z(z() , i)
   u(i) = z(i)*u(i-1)
   print " u(";i;") "
   prtv_v(u(), i)   
next i
'
'  end command acts as global destructor 
' of allocated memory ?
'
' With -exx option, no error messages, with or without end command.
'
end
'
' ======================================================================
'
'   Matrix math
'
'  1. dim as Matrix a
'  2. redim a.m( x1 to x2 , y1 to y2 )
'
'  3. dim as Matrix z(z1 to z2)  ?
'  4. redim z.m(w1 to w2)  ?
'
'     Matrix(x , y) ~= redim (0 to x-1, 0 to y-1) ?


' These declarations from :
'  http://www.rosettacode.org/wiki/Matrix_multiplication#FreeBASIC
' That content is available under
'  Content is available under GNU Free Documentation License 1.2 unless otherwise noted.
'
'   Using Geany, Build, Set Build Commands
'
'   Compile                fbc -w all "%f" 
'
'   Execute                "./%e" 
'
' Preliminary results checked using Maxima CAS
'
' also at, especially for vector by matrix calculations.
' https://keisan.casio.com/exec/system/15052033860538
'
' https://matrix.reshish.com/multCalculation.php
'
' https://elsenaju.eu/Calculator/matrix-vector-product.htm
'
'
'type Matrix
'    dim as double m( any , any )
'    declare constructor ( )
'    declare constructor ( byval x as uinteger , byval y as uinteger )
'end type
 
constructor Matrix ( )
end constructor
 
constructor Matrix ( byval x as uinteger , byval y as uinteger )
    redim this.m( x - 1 , y - 1 )
end constructor


'type Vector
'dim as double v( any )
'    declare constructor ( )
'    declare constructor ( byval x as uinteger )
'end type

constructor Vector ( )
end constructor

constructor Vector ( byval x as uinteger )
    redim this.v( x - 1 )
end constructor
'
' ====================================================================
'
Public operator * ( byref a as Matrix , byref b as Matrix ) as Matrix
'  Only applicable to square matrices or arrays.
' This routine from :
'  http://www.rosettacode.org/wiki/Matrix_multiplication#FreeBASIC
'
' Dodicat suggests only using: if ubound( a.m , 2 ) = ubound( b.m , 1 )
'
'
    dim as Matrix ret
    dim as uinteger i, j, k
    'if ubound( a.m , 2 ) = ubound( b.m , 1 ) and ubound( a.m , 1 ) = ubound( b.m , 2 ) then
     if ubound( a.m , 2 ) = ubound( b.m , 1 ) then
        redim ret.m( ubound( a.m , 1 ) , ubound( b.m , 2 ) )
        for i = 0 to ubound( a.m , 1 )
            for j = 0 to ubound( b.m , 2 )
                for k = 0 to ubound( b.m , 1 )
                    ret.m( i , j ) += a.m( i , k ) * b.m( k , j )
                next k
            next j
        next i
    end if
    return ret
end operator
'
'
Public operator * ( byref a as Matrix , byref b as Vector ) as Vector
'
'   Vector multiplied by a matrix, maybe correct .
'
'
   dim as Vector ret
   dim as uinteger i, j, k
   if ubound( a.m , 2 ) = ubound( b.v , 1 )  then
        redim ret.v( ubound( a.m , 1 ) )
        for i = 0 to ubound( a.m , 1 )
            
                for k = 0 to ubound( b.v , 1 )
                    ret.v( i ) += a.m( i , k ) * b.v( k  )
                next k
            
        next i
    end if   
   return ret
end operator
'
Public operator - ( byref a as Matrix , byref b as Matrix ) as Matrix
    dim as Matrix ret
    dim as uinteger i, j, k
    if ubound( a.m , 2 ) = ubound( b.m , 1 ) and ubound( a.m , 1 ) = ubound( b.m , 2 ) then
        redim ret.m( ubound( a.m , 1 ) , ubound( b.m , 2 ) )
        for i = 0 to ubound( a.m , 1 )
            for j = 0 to ubound( b.m , 2 )
                ret.m( i , j ) = a.m( i , j ) - b.m( i , j )
            next j
        next i
    end if
    return ret
end operator
'
Public operator - ( byref a as Vector , byref b as Vector ) as Vector
    dim as Vector ret
    dim as uinteger i
    if ubound( a.v , 1 ) = ubound( b.v , 1 ) then
        redim ret.v( ubound( a.v , 1 )  )
        for i = 0 to ubound( a.v , 1 )
            ret.v( i  ) = a.v( i  ) - b.v( i  )
        next i
    end if
    return ret
end operator
 '
Public operator + ( byref a as Matrix , byref b as Matrix ) as Matrix
    dim as Matrix ret
    dim as uinteger i, j, k
    if ubound( a.m , 2 ) = ubound( b.m , 1 ) and ubound( a.m , 1 ) = ubound( b.m , 2 ) then
        redim ret.m( ubound( a.m , 1 ) , ubound( b.m , 2 ) )
        for i = 0 to ubound( a.m , 1 )
            for j = 0 to ubound( b.m , 2 )
               ret.m( i , j ) = a.m( i , j ) + b.m( i , j )
            next j
        next i
    end if
    return ret
end operator
'
Public operator + ( byref a as Vector , byref b as Vector ) as Vector
    dim as Vector ret
    dim as uinteger i
    if ubound( a.v , 1 ) = ubound( b.v , 1 ) then
        redim ret.v( ubound( a.v , 1 )  )
        for i = 0 to ubound( a.v , 1 )
            ret.v( i  ) = a.v( i  ) + b.v( i  )
        next i
    end if
    return ret
end operator
'
Public operator / ( byref a as Matrix , byref b as Matrix ) as Matrix
    dim as Matrix ret
    dim as uinteger i, j, k
    dim as double x, y, z
    if ubound( a.m , 2 ) = ubound( b.m , 1 ) and ubound( a.m , 1 ) = ubound( b.m , 2 ) then
        redim ret.m( ubound( a.m , 1 ) , ubound( b.m , 2 ) )
        for i = 0 to ubound( a.m , 1 )
            for j = 0 to ubound( b.m , 2 )
                x = a.m( i , j )
                y = b.m( i , j )
                y = sgn(x)*10^32 ' assume y = 0
             if y <> 0 then z = x/y
                ret.m( i , j ) = z
            next j
        next i
    end if
    return ret
end operator
'
Public operator / ( byref a as Vector , byref b as Vector ) as Vector
    dim as Vector ret
    dim as uinteger i
    dim as double x, y, z
    if ubound( a.v , 1 ) = ubound( b.v , 1 ) then
        redim ret.v( ubound( a.v , 1 ) )
        for i = 0 to ubound( a.v , 1 )
                x = a.v( i )
                y = b.v( i )
                y = sgn(x)*10^32 ' assume y = 0
             if y <> 0 then z = x/y
                ret.v( i  ) = z
        next i
    end if
    return ret
end operator
'
' ......................................................................
'
Public sub prt_z(z() as Matrix, idx as Integer)
'
'     Matrix of Matrix, index and print .
'
dim as integer i,j
'
'print
for i=0 to ubound(z(idx).m,1)
 for j=0 to ubound(z(idx).m,2) 
    print Using "###.####";z(idx).m(i,j);" ";
  next j
  print
next i
print
'  
end sub
'
' ......................................................................
'
Public sub prt_m(z as Matrix)
'
'  Print elements from rectangular matrix .
'
dim as integer i,j
'

print
print ubound(z.m,1);" ";ubound(z.m,2)
print
 for i=0 to ubound(z.m,1) 
  for j=0 to ubound(z.m,2)  
    print Using "###.####";z.m(i,j);" ";
  next j
  print
next i
print
'  
end sub
'
' ......................................................................
'
Public sub prt_v(z as Vector)
'
'  Print elements from vector .
'
dim as integer i
'
'print
for i=0 to ubound(z.v,1)
    
    print Using "###.####";z.v(i);" ";
  
  
next i
print
print
'  
end sub
'
' ......................................................................
'
Public sub Vect_x_Matrix(M1 as Matrix, V1 as Vector, V3 as Vector)
'
'     Multiply vector by matrix .
'
dim as integer i,j,ub1, ub2
dim as single x,y,s
    
    ub1=ubound(M1.m,1)
    ub2=ubound(M1.m,2)
'    
    redim V3.v(0 to ub1)
'
        for j=0 to ub1
             s=0
         for i=0 to ub2
             x=V1.v(i)
             y=M1.m(j,i)
             s=s+x*y
         next i
          V3.v(j)=s
      next j
 '
end sub
'
' ......................................................................
'
Public sub gen_z(z() as Matrix, idx as Integer)
'
'     Matrix of Matrix, index and generate values .
'
dim as integer i,j
'
Randomize
  for j=0 to ubound(z(idx).m,2) 
   for i=0 to ubound(z(idx).m,1)
      z(idx).m(i,j) = -1 + 2*rnd
  next i
next j

'  
end sub
'
' ......................................................................
'
Public sub gen_v(u() as Vector, idx as Integer)
'
'     Vector of Vector, index and generate values .
'
dim as integer i,j
Randomize
'
for i=0 to ubound(u(idx).v,1)
   
      u(idx).v(i) = -1 + 2*rnd
  
next i

'  
end sub
'
' ......................................................................
'
Public sub prtv_v(u() as Vector, idx as Integer)
'
'     Vector of Vector, index and print values .
'
dim as integer i,j
'
for i=0 to ubound(u(idx).v,1)
   print Using "###.####";u(idx).v(i)
next i
print 
'  
end sub
'
' ......................................................................
'

end

#endmacro
#endif

