unit uLiss3DTypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, gl;

type
  TPoint3d = record
    x, y, z: Double;
  end;
  PPoint3d = ^TPoint3d;

  TPoint3dArray = array of TPoint3d;

  TMatrix4f = array[0..3, 0..3] of GLfloat;

function Point3d(x, y, z: Double): TPoint3d;

implementation

function Point3d(x, y, z: Double): TPoint3d;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

end.

