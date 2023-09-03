{ Generator for 3d Lissajous curves. }

unit uLiss3dGen;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpExprPars,
  uLiss3dTypes;

type
  TLiss3dGen = class
  private
    FParsers: array[0..2] of TFPExpressionParser;
    FFormulas: array[0..2] of string;
    FStepCount: Integer;
    FStepSize: Double;
    function GetCoeff(const AName: String): Double;
    function GetFormula(AIndex: Integer): String;
    procedure SetCoeff(const AName: String; AValue: Double);
    procedure SetFormula(AIndex: Integer; const AValue: String);
  protected

  public
    constructor Create;
    destructor Destroy; override;
    function Calculate: TPoint3dArray;
    procedure SetCoeffs(a, b, c, d: Double);
    procedure SetFormulas(const AFormulaX, AFormulaY, AFormulaZ: String);

    property FormulaX: string index 0 read GetFormula write SetFormula;
    property FormulaY: string index 1 read GetFormula write SetFormula;
    property FormulaZ: string index 2 read GetFormula write SetFormula;
    property Coeff[AName: String]: Double read GetCoeff write SetCoeff;
    property StepCount: Integer read FStepCount write FStepCount;
    property StepSize: Double read FStepSize write FStepSize;   // in degrees
  end;

implementation

constructor TLiss3dGen.Create;
var
  i: Integer;
begin
  // Create the expression parsers:
  // Index 0 for x, index 1 for y, index 2 for z variable.
  // Add the needed variables (t, a, b, c, d) to each parser. }
  for i := 0 to 2 do
  begin
    FParsers[i] := TFPExpressionParser.Create(nil);
    FParsers[i].BuiltIns := [bcMath];
    FParsers[i].Identifiers.AddFloatVariable('t', 0.0);
    FParsers[i].Identifiers.AddFloatVariable('a', 0.0);
    FParsers[i].Identifiers.AddFloatVariable('b', 0.0);
    FParsers[i].Identifiers.AddFloatVariable('c', 0.0);
    FParsers[i].Identifiers.AddFloatVariable('d', 0.0);
  end;

  FStepCount := 400;
  FStepSize := pi/180.0;    // Units of StepSize are angle degrees.
end;

destructor TLiss3dGen.Destroy;
var
  i: Integer;
begin
  for i := 0 to 2 do
    FParsers[i].Free;
  inherited;
end;

function TLiss3dGen.Calculate: TPoint3dArray;
var
  t: Double;
  i, j: Integer;
  P: TPoint3d;
  PArr: array[0..2] of double;
begin
  Result := nil;
  SetLength(Result, FStepCount);
  for i := 0 to FStepCount-1 do
  begin
    t := FStepSize * i;
    for j := 0 to 2 do
    begin
      FParsers[j].Identifiers.IdentifierByName('t').AsFloat := t;
      pArr[j] := ArgToFloat(FParsers[j].Evaluate);
    end;
    Result[i] := PPoint3D(@pArr)^;
  end;
end;

function TLiss3dGen.GetCoeff(const AName: String): Double;
begin
  Result := FParsers[0].Identifiers.FindIdentifier(AName).AsFloat;
end;

function TLiss3dGen.GetFormula(AIndex: Integer): String;
begin
  Result := FFormulas[AIndex];
end;

procedure TLiss3dGen.SetCoeff(const AName: String; AValue: Double);
var
  i: Integer;
begin
  for i := 0 to High(FParsers) do
    FParsers[i].Identifiers.IdentifierByName(AName).AsFloat := AValue;
end;

procedure TLiss3dGen.SetCoeffs(a, b, c, d: Double);
begin
  SetCoeff('a', a);
  SetCoeff('b', b);
  SetCoeff('c', c);
  SetCoeff('d', d);
end;

procedure TLiss3dGen.SetFormula(AIndex: Integer; const AValue: String);
begin
  FFormulas[AIndex] := AValue;
  FParsers[AIndex].Expression := AValue;
end;

procedure TLiss3dGen.SetFormulas(const AFormulaX, AFormulaY, AFormulaZ: String);
begin
  SetFormula(0, AFormulaX);
  SetFormula(1, AFormulaY);
  SetFormula(2, AFormulaZ);
end;

end.

