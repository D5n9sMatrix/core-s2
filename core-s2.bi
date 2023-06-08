#inclib "core-s2"
/'==============================================================================
A dual cone C * (S) is a closed convex cone. We see this by considering any
* *
v1 , v2 ? C * and real numbers a, b = 0. For any v ? S, it must be the case that
v1* , v = 0 and v2* , v = 0; hence, (av1* + bv2* ), v = 0, that 
is, av1* + bv2* ? C * ,
so C * is a convex cone. The closure property comes from the = condition in
the de?nition.
==============================================================================='/
declare sub DualCone ( )
#ifdef dual_cone
#macro cone ( convex_cone )
/'
Original Code:
// Round cone - exact
float sdRoundCone( vec3 p, float r1, float r2, float h )
{
  // sampling independent computations (only depend on shape)
  float b = (r1-r2)/h;
  float a = sqrt(1.0-b*b);

  // sampling dependant computations
  vec2 q = vec2( length(p.xz), p.y );
  float k = dot(q,vec2(-b,a));
  if( k<0.0 ) return length(q) - r1;
  if( k>a*h ) return length(q-vec2(0.0,h)) - r2;
  return dot(q, vec2(a,b) ) - r1;
}

Intermediate Code:
double _ZN12SYSTEM_BUS_T11SDROUNDCONEER7VECTOR3ddd( struct $12SYSTEM_BUS_T* THIS$1, struct $7VECTOR3* P$1, \
                                                    double R1$1, double R2$1, double H$1 )
{
 struct $7VECTOR2 TMP$1959$1;
 struct $7VECTOR2 TMP$1960$1;
 struct $7VECTOR2 TMP$1963$1;
 double fb$result$1;
 __builtin_memset( &fb$result$1, 0, 8ll );
 label$2277:;
 double B$1;
 B$1 = (R1$1 - R2$1) / H$1;
 double A$1;
 double vr$6 = _Z4SQRTd( -(B$1 * B$1) + 0x1.p+0 );
 A$1 = vr$6;
 struct $7VECTOR2 Q$1;
 struct $7VECTOR2* vr$9 = _ZN7VECTOR32XZEv( &TMP$1959$1, P$1 );
 double vr$10 = _Z6LENGTHRK7VECTOR2( (struct $7VECTOR2*)vr$9 );
 _ZN7VECTOR2C1Edd( &Q$1, vr$10, *(double*)((uint8*)P$1 + 8ll) );
 double K$1;
 _ZN7VECTOR2C1Edd( &TMP$1960$1, -B$1, A$1 );
 double vr$16 = _Z3DOTR7VECTOR2S0_( &Q$1, &TMP$1960$1 );
 K$1 = vr$16;
 if( K$1 >= 0x0p+0 ) goto label$2280;
 {
  double vr$18 = _Z6LENGTHRK7VECTOR2( (struct $7VECTOR2*)&Q$1 );
  fb$result$1 = vr$18 - R1$1;
  goto label$2278;
  label$2280:;
 }
 if( K$1 <= (A$1 * H$1) ) goto label$2282;
 {
  struct $7VECTOR2 TMP$1961$2;
  struct $7VECTOR2 TMP$1962$2;
  _ZN7VECTOR2C1Edd( &TMP$1961$2, 0x0p+0, H$1 );
  struct $7VECTOR2* vr$25 = _ZmiR7VECTOR2S0_( &TMP$1962$2, &Q$1, &TMP$1961$2 );
  double vr$26 = _Z6LENGTHRK7VECTOR2( (struct $7VECTOR2*)vr$25 );
  fb$result$1 = vr$26 - R2$1;
  goto label$2278;
  label$2282:;
 }
 _ZN7VECTOR2C1Edd( &TMP$1963$1, A$1, B$1 );
 double vr$31 = _Z3DOTR7VECTOR2S0_( &Q$1, &TMP$1963$1 );
 fb$result$1 = vr$31 - R1$1;
 goto label$2278;
 label$2278:;
 return fb$result$1;
}

FreeBASIC:
'/
proc SYSTEM_BUS_T.sdRoundCone( p as vector3, r1 as float, r2 as float, h as float) as float
  ' sampling independent computations (only depend on shape)
  dim as float b = (r1-r2)/h
  dim as float a = sqrt(1.0-b*b)

  ' sampling dependant computations
  dim as vector2 q = vector2( length(p.xz), p.y )
  dim as float k = dot(q,vector2(-b,a))
  if( k<0.0 ) then return length(q) - r1
  if( k>a*h ) then return length(q-vector2(0.0,h)) - r2
  return dot(q, vector2(a,b) ) - r1
end proc
end
#endmacro
#endif
