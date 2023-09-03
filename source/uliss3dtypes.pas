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

  ToglVector2f = array[0..1] of GLFloat;
  ToglVector3f = array[0..2] of GLFloat;
  ToglVector4f = array[0..3] of GLFloat;

implementation

end.

