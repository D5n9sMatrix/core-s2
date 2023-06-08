/'==============================================================================
From the de?nitions, it is clear in any case that the polar cone C 0 can be
formed by multiplying all of the vectors in the corresponding dual cone C *
by -1, and so C 0 = -C * .
==============================================================================='/
#if 0
#macro pole ( c0, c1, c2 )
declare sub pole ( byval c0 as CWSTR, byval c1 as CWSTR, byval c2 as CWSTR )
Type Vector
    Public:
        Declare Constructor()
        Declare Constructor(Byval n As Uinteger)
        Declare Operator [](Byval n As Uinteger) Byref As Double
        Declare Property UpperBound() As Integer
    Private:
        Dim As Double array(Any)
End Type

Constructor Vector()
End Constructor

Constructor Vector(Byval n As Uinteger)
    If n > 0 Then
        Redim This.array(n - 1)
    End If
End Constructor

Operator Vector.[](Byval n As Uinteger) Byref As Double
    Return This.array(n)
End Operator

Property Vector.UpperBound() As Integer
    Return Ubound(This.array)
End Property

Type Matrix
    Public:
        Declare Constructor()
        Declare Constructor(Byval n1 As Uinteger, Byval n2 As Uinteger)
        Declare Operator [](Byval n As Uinteger) Byref As Vector
        Declare Property UpperBound1() As Integer
        Declare Property UpperBound2() As Integer
    Private:
        Dim As Vector array(Any)
End Type

Constructor Matrix()
End Constructor

Constructor Matrix(Byval n1 As Uinteger, Byval n2 As Uinteger)
    If n1 > 0 And n2 > 0 Then
        Redim This.array(n1 - 1)
        For i As Integer = 0 To n1 - 1
            This.array(i) = Vector(n2)
        Next I
    End If
End Constructor

Operator Matrix.[](Byval n As Uinteger) Byref As Vector
    Return This.array(n)
End Operator

Property Matrix.UpperBound1() As Integer
    Return Ubound(This.array)
End Property

Property Matrix.UpperBound2() As Integer
    If Ubound(This.array) >= 0 Then
        Return This.array(0).UpperBound
    Else
        Return -1
    End If
End Property

Operator *(Byref M As Matrix, Byref V As Vector) As Vector
    If M.UpperBound2 = V.UpperBound Then
        Dim As Vector R = Vector(M.UpperBound1 + 1)
        For i As Integer = 0 To M.UpperBound1
            For k As Integer = 0 To V.UpperBound
                R[i] += M[i][k] * V[k]
            Next k
        Next i
        Return R
    Else
        Error 1
    End If
End Operator

Operator *(Byref M1 As Matrix, Byref M2 As Matrix) As Matrix
    If M1.UpperBound2 = M2.UpperBound1 Then
        Dim As Matrix R = Matrix(M1.UpperBound1 + 1, M2.UpperBound2 + 1)
        For i As Integer = 0 To M1.UpperBound1
            For j As Integer = 0 To M2.UpperBound2
                For k As Integer = 0 To M1.UpperBound2
                    R[i][j] += M1[i][k] * M2[k][j]
                Next k
            Next j
        Next i
        Return R
    Else
        Error 1
    End If
End Operator

'-----------------------------------------------------------------------------

Dim As Vector V = Vector(3)
V[0] = 1
V[1] = 2
V[2] = 3
For i As Integer = 0 To V.UpperBound
    Print V[i]
Next i
Print

Dim As Matrix M1 = Matrix(3, 2)
M1[0][0]= 1 : M1[0][1] = 2
M1[1][0]= 3 : M1[1][1] = 4
M1[2][0]= 5 : M1[2][1] = 6
For i As Integer = 0 To M1.UpperBound1
    For j As Integer = 0 To M1.UpperBound2
        Print M1[i][j]; " ";
    Next j
    Print
Next i
Print

Dim As Matrix M2 = Matrix(2, 3)
M2[0][0]= 1 : M2[0][1] = 2 : M2[0][2] = 3
M2[1][0]= 4 : M2[1][1] = 5 : M2[1][2] = 6
For i As Integer = 0 To M2.UpperBound1
    For j As Integer = 0 To M2.UpperBound2
        Print M2[i][j]; " ";
    Next j
    Print
Next i
Print

Dim As Matrix Mx = M1 * M2
For i As Integer = 0 To Mx.UpperBound1
    For j As Integer = 0 To Mx.UpperBound2
        Print Mx[i][j]; " ";
    Next j
    Print
Next i
Print

Dim As Vector Vx = Mx * V
For i As Integer = 0 To Vx.UpperBound
    Print Vx[i]
Next i
Print

Sleep

end
#endmacro
#endif