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

implementation

end.

