{ Generator for 3d Lissajous curves. }

unit uLiss3dGen;

{$mode Delphi}

interface

uses
  Classes, SysUtils,  uLiss3dTypes;

type
  TLissCoeffs = array of Double;
  TLissFormula = procedure(t: Double; const ACoeffs: TLissCoeffs; out P: TPoint3D) of object;

  TLiss3dGen = class
  private
    FCoeffs: TLissCoeffs;
    FStepCount: Integer;
    FStepSize: Double;
    FFormula: TLissFormula;
  protected

  public
    Points: TPoint3dArray;

    constructor Create;
    destructor Destroy; override;
    procedure Calculate;
    procedure SetCoeffs(const AValue: TLissCoeffs);
    property Formula: TLissFormula read FFormula write FFormula;
    property StepCount: Integer read FStepCount write FStepCount;
    property StepSize: Double read FStepSize write FStepSize;
  end;

implementation

constructor TLiss3dGen.Create;
begin
  SetLength(FCoeffs, 4);
  FCoeffs[0] := 1.0;
  FCoeffs[1] := 0.0;
  FCoeffs[2] := 0.0;
  FCoeffs[3] := 0.0;
  FStepCount := 400;
  FStepSize := pi/180.0;
end;

destructor TLiss3dGen.Destroy;
begin
  inherited;
end;

procedure TLiss3dGen.Calculate;
const
  EPS = 1E-9;
var
  t: Double;
  i: Integer;
  P: TPoint3d;
begin
  if not Assigned(FFormula) then
    exit;

  SetLength(Points, FStepCount);
  for i := 0 to FStepCount-1 do
  begin
    t := FStepSize * i;
    FFormula(t, FCoeffs, P);
    Points[i] := P;
    {
    if (i > 0) and
       SameValue(P.X, Points[0].X, EPS) and
       SameValue(P.Y, Points[0].Y, EPS) and
       SameValue(P.Z, Points[0].Z, EPS)
    then
      exit;
      }
  end;
end;

procedure TLiss3dGen.SetCoeffs(const AValue: TLissCoeffs);
var
  i: Integer;
begin
  SetLength(FCoeffs, Length(AValue));
  for i := Low(AValue) to High(AValue) do
    FCoeffs[i] := AValue[i];
end;

end.

