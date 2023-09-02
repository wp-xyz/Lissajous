unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Spin,
  gl, glu, OpenGLContext,
  uLiss3dTypes, uLiss3dGen, uLiss3dViewGL;

type
  TLoadSave = array[0..15] of DWord;

  { TMainForm }

  TMainForm = class(TForm)
    cbSymbolColor: TColorButton;
    cbShowAxes: TCheckBox;
    cbBackgroundColor: TColorButton;
    cbTeleLens: TCheckBox;
    ColorButton1: TColorButton;
    cbViewDirection: TComboBox;
    GroupBox3: TGroupBox;
    ImageList: TImageList;
    lblX: TLabel;
    lblY: TLabel;
    lblZ: TLabel;
    lblClosingBracket: TLabel;
    lblDegrees: TLabel;
    lblCoeffA: TLabel;
    lblCoeffD: TLabel;
    lblCoeffC: TLabel;
    lblCoeffB: TLabel;
    lblFormula3X: TLabel;
    lblFormula3Y: TLabel;
    lblFormula2Z: TLabel;
    lblFormula2X: TLabel;
    lblFormula2Y: TLabel;
    lblFormula3Z: TLabel;
    lblStepSize: TLabel;
    lblStepCount: TLabel;
    OpenDialog: TOpenDialog;
    Panel4: TPanel;
    Panel5: TPanel;
    rbFormula2: TRadioButton;
    rbFormula3: TRadioButton;
    SaveDialog: TSaveDialog;
    seCoeffA: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    lblFormula1X: TLabel;
    lblFormula1Y: TLabel;
    lblFormula1Z: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    rbFormula1: TRadioButton;
    seCoeffD: TFloatSpinEdit;
    seCoeffC: TFloatSpinEdit;
    seCoeffB: TFloatSpinEdit;
    seStepSize: TFloatSpinEdit;
    seStepCount: TSpinEdit;
    ToolBar1: TToolBar;
    tbLoadParams: TToolButton;
    tbSaveParams: TToolButton;
    tbSaveAsBitmap: TToolButton;
    procedure cbBackgroundColorColorChanged(Sender: TObject);
    procedure cbShowAxesChange(Sender: TObject);
    procedure cbSymbolColorColorChanged(Sender: TObject);
    procedure cbTeleLensChange(Sender: TObject);
    procedure cbViewDirectionChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rbFormulaChange(Sender: TObject);
    procedure tbLoadParamsClick(Sender: TObject);
    procedure tbSaveAsBitmapClick(Sender: TObject);
    procedure tbSaveParamsClick(Sender: TObject);
    procedure UpdateLissajousHandler(Sender: TObject);
  private
    FActivated: Boolean;
    FGenerator: TLiss3dGen;
    FViewer: TLiss3dViewerFrame;
    procedure UpdateCoeffState;
    procedure UpdateLissParams;
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

  private
    // Parameters
    procedure PackFile(out AData: TLoadSave);
    procedure UnpackFile(AData: TLoadSave);

  private
    (*
    // OpenGL
    FInitDone: Boolean;
    FCameraDistance: GLFloat;
    FCameraAngleX, FCameraAngleY, FCameraAngleZ: GLFloat;
    FMouseX, FMouseY: GLFloat;
    FSphere: PGLUQuadric;
    FSymbolSize: Double;
    procedure InitGL;
    procedure InitLights;
    procedure DrawAxes;
    procedure DrawScene;
    *)
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  FPImage, GraphType;

const
  CAMERA_ANGLE = 45.0;
  TELE_DISTANCE_FACTOR = 2.0;
  TELE_ANGLE = 10.0;

  SIGNATURE = 'lissa';

  IncValues: array[1..13] of single = (
    0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10
  );
  ColorList: array[0..6] of dword =
    ($ffffff, $ff0000, $00ff00, $0000ff, $ffff00, $ff00ff, $00ffff);

procedure Formula1(t: Double; const ACoeffs: TLissCoeffs; out P: TPoint3D);
begin
  P.X := cos(ACoeffs[0] * t);
  P.Y := sin(ACoeffs[1] * t);
  P.Z := sin(ACoeffs[2] * t);
end;

procedure Formula2(t: Double; const ACoeffs: TLissCoeffs; out P: TPoint3D);
begin
  P.X := (cos(ACoeffs[0] * t) + cos(ACoeffs[1] * t)) / 2;
  P.Y := (sin(ACoeffs[0] * t) + sin(ACoeffs[2] * t)) / 2;
  P.Z := sin(ACoeffs[3] * t);
end;

procedure Formula3(t: Double; const ACoeffs: TLissCoeffs; out P: TPoint3D);
begin
  P.X := sin(ACoeffs[0] * t) * ( 1.0 + cos(ACoeffs[1] * t)) / 2;
  P.Y := sin(ACoeffs[0] * t) * ( 1.0 + sin(ACoeffs[2] * t)) / 2;
  P.Z := sin(ACoeffs[3] * t);
end;


{ TMainForm }

procedure TMainForm.cbBackgroundColorColorChanged(Sender: TObject);
begin
  FViewer.BackColor := cbBackgroundColor.ButtonColor;
end;

procedure TMainForm.cbShowAxesChange(Sender: TObject);
begin
  FViewer.ShowAxes := cbShowAxes.Checked;
end;

procedure TMainForm.cbSymbolColorColorChanged(Sender: TObject);
begin
  FViewer.SymbolColor := cbSymbolColor.ButtonColor;
end;

procedure TMainForm.cbTeleLensChange(Sender: TObject);
begin
  if cbTeleLens.Checked then
  begin
    FViewer.CameraDistance := FViewer.CameraDistance * TELE_DISTANCE_FACTOR;
    FViewer.CameraAngle := TELE_ANGLE;
  end else
  begin
    FViewer.CameraDistance := FViewer.CameraDistance / TELE_DISTANCE_FACTOR;
    FViewer.CameraAngle := CAMERA_ANGLE;
  end;
  FViewer.InvalidateView;
end;

procedure TMainForm.cbViewDirectionChange(Sender: TObject);
begin
  case cbViewDirection.ItemIndex of
    1: begin    // xy plane
         FViewer.CameraRotX := 0;
         FViewer.CameraRotY := 0;
         FViewer.CameraRotZ := 0;
       end;
    2: begin    // yz plane
         FViewer.CameraRotX := -90;
         FViewer.CameraRotY := 0;
         FViewer.CameraRotZ := 0;
       end;
    3: begin    // xz plane
         FViewer.CameraRotX := -90;
         FViewer.CameraRotY := 0;
         FViewer.CameraRotZ := -90;
       end;
    4: begin    // diagonal
         FViewer.CameraRotX := 45;
         FViewer.CameraRotY := -45;
         FViewer.CameraRotZ := 0;
       end;
  end;
  FViewer.InvalidateView;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if not FActivated then
  begin
    FActivated := true;
    UpdateLissajousHandler(nil);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FGenerator := TLiss3dGen.Create;

  FViewer := TLiss3dViewerFrame.Create(self);
  FViewer.Parent := Panel2;
  FViewer.Align := alClient;
  FViewer.OnMouseMove := @ViewerMouseMove;

  UpdateCoeffState;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FGenerator.Free;
end;

procedure TMainForm.PackFile(out AData: TLoadSave);
type
  PDW = ^dword;
var
  i: byte;
  x: Single;
begin
  AData := Default(TLoadSave);
  for i := 0 to 4 do
    AData[i] := byte(SIGNATURE[i+1]);

  if rbFormula1.Checked then
    AData[5] := 1
  else if rbFormula2.Checked then
    AData[5] := 2
  else if rbFormula3.Checked then
    AData[5] := 3;

  AData[6] := 1;

  AData[7] := cbSymbolColor.ButtonColor;
  for i := Low(ColorList) to High(ColorList) do
    if ColorList[i] = cbSymbolColor.ButtonColor then
      AData[7] := i;

  AData[8] := 0;  // Boolean2Int(smooth);
  AData[9] := 0;  // Boolean2Int(grid);
  AData[10] := seStepCount.Value;

  x := -seStepSize.Value;
  AData[11] := PDW(@x)^;
  for i := Low(IncValues) to High(IncValues) do
    if IncValues[i] = seStepSize.Value then
      AData[11] := i;

  x := seCoeffA.Value;  AData[12] := PDW(@x)^;
  x := seCoeffB.Value;  AData[13] := PDW(@x)^;
  x := seCoeffC.Value;  AData[14] := PDW(@x)^;
  x := seCoeffD.Value;  AData[15] := PDW(@x)^;
end;

procedure TMainForm.rbFormulaChange(Sender: TObject);
begin
  UpdateCoeffState;
  UpdateLissajousHandler(nil);
end;

procedure TMainForm.tbLoadParamsClick(Sender: TObject);
var
  F: file of TloadSave;
  data: TLoadSave;
begin
  with OpenDialog do
    if Execute then
      try
        AssignFile(F, Filename);
        Reset(F);
        Read(F, data);
        UnpackFile(data);
      finally
        CloseFile(F);
      end;
end;

procedure TMainForm.tbSaveAsBitmapClick(Sender: TObject);
var
  ext: string;
  bm: TCustomBitmap;
begin
  with SaveDialog do
  begin
    Filter := 'BMP files|*.bmp|PNG files|*.png|JPEG files|*.jpg;*.jpeg';
    FilterIndex := 2;
    if Execute then
    begin
      ext := ExtractFileExt(FileName);
      case ext of
        '.bmp': bm := TBitmap.Create;
        '.png': bm := TPortableNetworkGraphic.Create;
        '.jpg', '.jpeg': bm := TJpegImage.Create;
        else raise Exception.Create('Image type not supported.');
      end;
      try
        FViewer.ToBitmap(bm);
        bm.SaveToFile(FileName);
      finally
        bm.Free;
      end;
    end;
  end;
end;

// Save settings of drawing
procedure TMainForm.tbSaveParamsClick(Sender: TObject);
var
  F: File of TLoadSave;
  data: TLoadSave;
begin
  PackFile(data);
  with SaveDialog do
  begin
    Filter := 'Lissajous files|*.l3d';
    DefaultExt := '.l3d';
    if Execute then
      try
        AssignFile(F, FileName);
        ReWrite(F);
        Write(F, data);
      finally
        CloseFile(F);
      end;
  end;
end;

procedure TMainForm.UnpackFile(AData: TLoadSave);
type
  PS = ^Single;
var
  s: string;
  i: byte;
  penNr: byte;
  penColor: Integer;
  smooth: boolean;
  grid: Boolean;
  stepCount: Integer;
  incIdX: Integer;
  incSize: Single;
begin
  s := '';
  for i := 0 to 4 do s := s + chr(AData[i]);
  if (s <> SIGNATURE) then
  begin
    MessageDlg('No lissajous parameter file.', mtError, [mbOK], 0);
    exit;
  end;

  // Formula
  case AData[5] of
    1: rbFormula1.Checked := true;
    2: rbFormula2.Checked := true;
    3: rbFormula3.Checked := true;
    else rbFormula1.Checked := true;
  end;

  penNr := AData[6];

  // symbol color
  pencolor := AData[7];
  if (penColor >= Low(ColorList)) and (penColor <= High(ColorList)) then
    cbSymbolColor.ButtonColor := ColorList[penColor]
  else
    cbSymbolColor.ButtonColor := penColor;

  smooth := (AData[8] = 1);
  grid := AData[9] = 1;

  // Step count
  seStepCount.Value := AData[10];

  // Step size
  incSize := PS(@AData[11])^;
  if incSize < 0 then
    seStepSize.Value := incSize
  else
  begin
    incIdx := AData[11];
    if (incIdx >= 0) and (incIdx <= High(IncValues)) then
      seStepSize.Value := incValues[incIdx]
    else
      seStepSize.Value := 1.0;
  end;

  // Coefficients
  seCoeffA.Value := PS(@AData[12])^;
  seCoeffB.Value := PS(@AData[13])^;
  seCoeffC.Value := PS(@AData[14])^;
  seCoeffD.Value := PS(@AData[15])^;
end;

procedure TMainForm.UpdateCoeffState;
begin
  seCoeffD.Enabled := not rbFormula1.Checked;
  lblCoeffD.Enabled := not rbFormula1.Checked;
end;

procedure TMainForm.UpdateLissajousHandler(Sender: TObject);
begin
  UpdateLissParams;
  FGenerator.Calculate;
  FViewer.Points := FGenerator.Points;
  FViewer.Invalidate;
end;

procedure TMainForm.UpdateLissParams;
begin
  FGenerator.SetCoeffs([seCoeffA.Value, seCoeffB.Value, seCoeffC.Value, seCoeffD.Value]);
  FGenerator.StepCount := seStepCount.Value;
  FGenerator.StepSize := seStepSize.Value * pi/180;
  if rbFormula1.Checked then
    FGenerator.Formula := @Formula1
  else if rbFormula2.Checked then
    FGenerator.Formula := @Formula2
  else if rbFormula3.Checked then
    FGenerator.Formula := @Formula3;
end;

procedure TMainForm.ViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ([ssLeft, ssRight] * Shift <> []) then
    cbViewDirection.ItemIndex := 0;
end;

end.

