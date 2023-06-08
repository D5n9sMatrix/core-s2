/'==============================================================================
Figure 2.3. A set of vectors {v1 , v2 , v3 }, and the corresponding convex cone C, the
dual cone C * , and the polar cone C 0
2.2.8.3 Polar Cones
Given a set of vectors S in a given vector space (in cases of interest, S is
usually a cone, but not necessarily), the polar cone of S, denoted C 0 (S), is
de?ned as
C 0 (S) = {v 0 s.t. v 0 , v = 0 for all v ? S}.
==============================================================================='/
#ifdef top_like_key
#macro like_key ( set_vectors_space )
declare sub TopLikeKey ( )


'
'  matx_smv.bas
'
'   Successive matrix vector multiplications .
'

'' compile with: fbc matx_test.bas


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


'  end command acts as global destructor 
' of allocated memory ?

end
'
' ======================================================================
'
end
#endmacro
#endif
