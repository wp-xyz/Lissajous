unit uMain;

{$mode objfpc}{$H+}

interface

uses                                                  lazloggerbase,
  Classes, SysUtils, LCLType, LCLIntf, Types, IniFiles,
  Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Spin, Buttons, Menus, ActnList, StdActns,
  mrumanager, gl, glu,
  uLiss3dGen, uLiss3dViewGL;

type
  TLissFileRec = array[0..15] of DWord;

  { TMainForm }

  TMainForm = class(TForm)
    acCalculate: TAction;
    acLoadParams: TAction;
    acSaveParams: TAction;
    acSaveImage: TAction;
    acResetParams: TAction;
    ActionList: TActionList;
    ApplicationProperties: TApplicationProperties;
    cbSymbolColor: TColorButton;
    cbShowAxes: TCheckBox;
    cbBackgroundColor: TColorButton;
    cbStickColor: TColorButton;
    cbShowSticks: TCheckBox;
    cbShowSymbols: TCheckBox;
    cbTransparent: TCheckBox;
    ColorButton1: TColorButton;
    cbViewDirection: TComboBox;
    cbViewAngle: TComboBox;
    edFormulaX: TComboBox;
    edFormulaY: TComboBox;
    edFormulaZ: TComboBox;
    acExit: TFileExit;
    lblZDegrees: TLabel;
    lblXRotation: TLabel;
    lblYRotation: TLabel;
    lblXDegrees: TLabel;
    lblYDegrees: TLabel;
    lblZRotation: TLabel;
    seCameraDistance: TFloatSpinEdit;
    lblCameraDistance: TLabel;
    lblStickDiam: TLabel;
    LblSymbolDiam: TLabel;
    seXRotation: TFloatSpinEdit;
    seSymbolSize: TFloatSpinEdit;
    gbRendering: TGroupBox;
    ImageList: TImageList;
    lblViewAngle: TLabel;
    lblViewDirection: TLabel;
    lblXEquals: TLabel;
    lblYEquals: TLabel;
    lblZEquals: TLabel;
    lblX: TLabel;
    lblY: TLabel;
    lblZ: TLabel;
    lblClosingBracket: TLabel;
    lblStepDegrees: TLabel;
    lblCoeffA: TLabel;
    lblCoeffD: TLabel;
    lblCoeffC: TLabel;
    lblCoeffB: TLabel;
    lblStepSize: TLabel;
    lblStepCount: TLabel;
    OpenDialog: TOpenDialog;
    RecentFilesPopup: TPopupMenu;
    SaveDialog: TSaveDialog;
    seCoeffA: TFloatSpinEdit;
    gbExpressions: TGroupBox;
    gbCoefficients: TGroupBox;
    ParamsPanel: TPanel;
    ParamPanel: TPanel;
    seCoeffD: TFloatSpinEdit;
    seCoeffC: TFloatSpinEdit;
    seCoeffB: TFloatSpinEdit;
    seStepSize: TFloatSpinEdit;
    seStepCount: TSpinEdit;
    seStickDiam: TFloatSpinEdit;
    seYRotation: TFloatSpinEdit;
    seZRotation: TFloatSpinEdit;
    Splitter: TSplitter;
    ToolBar: TToolBar;
    tbLoadParams: TToolButton;
    tbSaveParams: TToolButton;
    tbSaveAsBitmap: TToolButton;
    tbReset: TToolButton;
    tbExit: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    tbCalculate: TToolButton;
    ToolButton3: TToolButton;
    procedure acCalculateExecute(Sender: TObject);
    procedure acLoadParamsExecute(Sender: TObject);
    procedure acResetParamsExecute(Sender: TObject);
    procedure acSaveImageExecute(Sender: TObject);
    procedure acSaveParamsExecute(Sender: TObject);
    procedure ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
    procedure cbBackgroundColorColorChanged(Sender: TObject);
    procedure cbShowSticksChange(Sender: TObject);
    procedure cbStickColorColorChanged(Sender: TObject);
    procedure cbTransparentChange(Sender: TObject);
    procedure cbViewAngleChange(Sender: TObject);
    procedure cbShowAxesChange(Sender: TObject);
    procedure cbSymbolColorColorChanged(Sender: TObject);
    procedure cbViewDirectionChange(Sender: TObject);
    procedure cbShowSymbolsChange(Sender: TObject);
    procedure cbViewDirectionDropDown(Sender: TObject);
    procedure edFormulaDropDown(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure seCameraDistanceChange(Sender: TObject);
    procedure seStickDiamChange(Sender: TObject);
    procedure seSymbolSizeChange(Sender: TObject);
    procedure seXRotationChange(Sender: TObject);
    procedure seYRotationChange(Sender: TObject);
    procedure seZRotationChange(Sender: TObject);
    procedure UpdateLissajousHandler(Sender: TObject);
  private
    FActivated: Boolean;
    FGenerator: TLiss3dGen;
    FViewer: TLiss3dViewerFrame;
    FViewerLock: Integer;
    function CalcComboBoxItemWidth(AComboBox: TComboBox): Integer;
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

  private
    // Formulas
    procedure AddBuiltinFormulas;
    procedure Calculate;
    procedure UpdateFormulaHistory;

  private
    // Parameters
    procedure LoadLissParams(ini: TCustomIniFile);
    procedure LoadParamFile(const AFileName: String);
    procedure SaveLissParams(ini: TCustomIniFile);
    function UnpackParams(AData: TLissFileRec): Boolean;
    procedure UpdateLissParams;

  private
    // Recently used files
    FRecentFilesManager: TMRUMenuManager;
    procedure RecentFileHandler(Sender: TObject; const AFileName: String);

  private
    FFileName: String;
    // Config
    function GetIniFileName: String;
    procedure ReadFromIni;
    procedure WriteToIni;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  TypInfo, Math;

const
  APP_NAME = 'Lissajous Curve Generator';
  MAX_FORMULA_COUNT = 20;
  MAX_MRU_FILE_COUNT = 20;
  MAX_DROPDOWN_COUNT = 20;
  SIGNATURE = 'lissa';

  IncValues: array[1..13] of single = (
    0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10
  );
  ColorList: array[0..6] of dword =
    ($ffffff, $ff0000, $00ff00, $0000ff, $ffff00, $ff00ff, $00ffff);

  BUILTIN_FORMULA1_X = 'cos(a*t)';
  BUILTIN_FORMULA1_Y = 'sin(b*t)';
  BUILTIN_FORMULA1_Z = 'sin(c*t)';

  BUILTIN_FORMULA2_X = '(cos(a*t)+cos(b*t))/2';
  BUILTIN_FORMULA2_Y = '(sin(a*t)+sin(c*t))/2';
  BUILTIN_FORMULA2_Z = 'sin(d*t)';

  BUILTIN_FORMULA3_X = 'sin(a*t)*(1.0+cos(b*t))/2';
  BUILTIN_FORMULA3_Y = 'sin(a*t)*(1.0+sin(c*t))/2';
  BUILTIN_FORMULA3_Z = 'sin(d*t)';

{ TMainForm }

procedure TMainForm.acCalculateExecute(Sender: TObject);
begin
  Calculate;
end;

procedure TMainForm.acLoadParamsExecute(Sender: TObject);
begin
  with OpenDialog do
  begin
    Filter := 'Lissajous 3D files|*.l3d';
    DefaultExt := '.l3d';
    InitialDir := ExtractFileName(FFileName);
    FileName := '';
    if Execute then
      LoadParamFile(FileName);
  end;
end;

procedure TMainForm.acResetParamsExecute(Sender: TObject);
begin
  seCoeffA.Value := 0.0;
  seCoeffB.Value := 0.0;
  seCoeffC.Value := 0.0;
  seCoeffD.value := 0.0;
  seStepCount.Value := 0;
  seStepSize.Value := 1.0;
  cbSymbolColor.ButtonColor := SYMBOL_COLOR;
  cbBackgroundColor.ButtonColor := clBlack;
  cbViewDirection.ItemIndex := 1;
  cbShowAxes.Checked := false;
  FViewer.CameraDistance := CAMERA_DISTANCE;
  FViewer.CameraAngle := VIEW_ANGLE;
  FViewer.CameraRotX := 0;
  FViewer.CameraRotY := 0;
  FViewer.CameraRotZ := 0;
  FViewer.ShowAxes := cbShowAxes.Checked;
  FViewer.SymbolColor := cbSymbolColor.ButtonColor;
  FViewer.BackColor := cbBackgroundColor.ButtonColor;
  FFileName := '';
end;

procedure TMainForm.acSaveImageExecute(Sender: TObject);
var
  ext: string;
  bm: TCustomBitmap;
begin
  with SaveDialog do
  begin
    Filter := 'BMP files|*.bmp|PNG files|*.png|JPEG files|*.jpg;*.jpeg';
    FilterIndex := 2;
    FileName := ChangefileExt(ExtractFilename(FFilename), '');
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
procedure TMainForm.acSaveParamsExecute(Sender: TObject);
var
  ini: TCustomIniFile;
begin
  with SaveDialog do
  begin
    Filter := 'Lissajous 3D files|*.l3d';
    DefaultExt := '.l3d';
    FileName := ExtractFileName(FFileName);
    InitialDir := ExtractFileDir(FFilename);
    if Execute then
    begin
      ini := TIniFile.Create(FileName);
      try
        SaveLissParams(ini);
        FRecentFilesManager.AddToRecent(FileName);
        FFileName := FileName;
      finally
        ini.Free;
      end;
    end;
  end;
end;

procedure TMainForm.AddBuiltinFormulas;

  procedure AddBuiltIn(ACombobox: TCombobox; AFormula: String);
  begin
    if ACombobox.Items.Indexof(AFormula) = -1 then
      ACombobox.Items.Add(AFormula);
  end;

begin
  AddBuiltin(edFormulaX, BUILTIN_FORMULA1_X);
  AddBuiltIn(edFormulaX, BUILTIN_FORMULA2_X);
  AddBuiltIn(edFormulaX, BUILTIN_FORMULA3_X);

  AddBuiltin(edFormulaY, BUILTIN_FORMULA1_Y);
  AddBuiltIn(edFormulaY, BUILTIN_FORMULA2_Y);
  AddBuiltIn(edFormulaY, BUILTIN_FORMULA3_Y);

  AddBuiltin(edFormulaZ, BUILTIN_FORMULA1_Z);
  AddBuiltIn(edFormulaZ, BUILTIN_FORMULA2_Z);
  AddBuiltIn(edFormulaZ, BUILTIN_FORMULA3_Z);
end;

procedure TMainForm.ApplicationPropertiesIdle(Sender: TObject;
  var Done: Boolean);
begin
  inc(FViewerLock);
  seCameraDistance.Value := FViewer.CameraDistance;
  seXRotation.Value := FViewer.CameraRotX;
  seYRotation.Value := FViewer.CameraRotY;
  seZRotation.Value := FViewer.CameraRotZ;
  dec(FViewerLock);
end;

function TMainForm.CalcComboBoxItemWidth(AComboBox: TComboBox): Integer;
var
  bmp: TBitmap;
  i, w: Integer;
begin
  bmp := TBitmap.Create;
  try
    bmp.SetSize(1, 1);
    bmp.Canvas.Font.Assign(ACombobox.Font);
    w := 0;
    for i := 0 to ACombobox.Items.Count-1 do
      w := Max(w, bmp.Canvas.TextWidth(ACombobox.Items[i]));
    Result := w + 20;
  finally
    bmp.Free;
  end;
end;

procedure TMainForm.Calculate;
begin
  if FViewerLock = 0 then
  begin
    UpdateLissParams;
    FViewer.Points := FGenerator.Calculate;
    FViewer.Invalidate;
    UpdateFormulaHistory;
  end;
end;

procedure TMainForm.cbBackgroundColorColorChanged(Sender: TObject);
begin
  FViewer.BackColor := cbBackgroundColor.ButtonColor;
end;

procedure TMainForm.cbShowAxesChange(Sender: TObject);
begin
  FViewer.ShowAxes := cbShowAxes.Checked;
end;

procedure TMainForm.cbShowSticksChange(Sender: TObject);
begin
  FViewer.ShowSticks := cbShowSticks.Checked;
end;

procedure TMainForm.cbStickColorColorChanged(Sender: TObject);
begin
  FViewer.StickColor := cbStickColor.ButtonColor;
end;

procedure TMainForm.cbTransparentChange(Sender: TObject);
begin
  if cbTransparent.Checked then
    FViewer.BackAlpha := 0.0
  else
    FViewer.BackAlpha := 1.0;
  FViewer.Invalidate;
end;

procedure TMainForm.cbSymbolColorColorChanged(Sender: TObject);
begin
  FViewer.SymbolColor := cbSymbolColor.ButtonColor;
end;

procedure TMainForm.cbViewAngleChange(Sender: TObject);
var
  angle: Double;
begin
  if cbViewAngle.ItemIndex = -1 then
    exit;
  if cbViewAngle.ItemIndex = cbViewAngle.Items.Count - 1 then
    FViewer.Projection := oglOrthographic
  else
  begin
    FViewer.Projection := oglPerspective;
    angle := StrToFloatDef(cbViewAngle.Items[cbViewAngle.ItemIndex].Split(' ')[0], 45);
    FViewer.CameraAngle := angle;
    FViewer.InvalidateView;
  end;
end;

procedure TMainForm.cbViewDirectionChange(Sender: TObject);
const
  ROTATIONS: array[0..18, 0..2] of double = (
    (  0,   0,   0),      // mouse

    (  0,   0,   0),      // xy plane (x right, y up)
    (  0, 180,   0),      // xy plane (x left, y up)

    (-90,   0, -90),      // yz plane (y right, z up)
    (-90,   0,  90),      // yz plane (y left, z up)

    (-90,   0,   0),      // xz plane (x right, z up)
    (-90,   0, 180),      // xz plane (x left, z up)

    ( 45,  45,  90),      // diagonal 1 (x up)
    ( 45, 135,  90),      // diagonal 2 (x up)
    ( 45, 225,  90),      // diagonal 3 (x up)
    ( 45, 315,  90),      // diagonal 4 (x up)

    ( 45, -45,   0),      // diagonal 5 (y up)
    ( 45,  45,   0),      // diagonal 6 (y up)
    ( 45, 135,   0),      // diagonal 7 (y up)
    ( 45, 225,   0),      // diagonal 8 (y up)

    (-45,   0, 225),      // diagonal 9 (z up)
    (-45,   0, -45),      // diagonal 10 (z up)
    (-45,   0,  45),      // diagonal 11 (z up)
    (-45,   0, 135)       // diagonal 12 (z up)
  );

begin
  FViewer.CameraRotX := ROTATIONS[cbViewDirection.ItemIndex, 0];
  FViewer.CameraRotY := ROTATIONS[cbViewDirection.ItemIndex, 1];
  FViewer.CameraRotZ := ROTATIONS[cbViewDirection.ItemIndex, 2];
  FViewer.InvalidateView;
end;

procedure TMainForm.cbShowSymbolsChange(Sender: TObject);
begin
  FViewer.ShowSymbols := cbShowSymbols.Checked;
end;

procedure TMainForm.cbViewDirectionDropDown(Sender: TObject);
begin
  if Sender is TComboBox then
    TComboBox(Sender).ItemWidth := CalcComboBoxItemWidth(TComboBox(Sender));
end;

procedure TMainForm.edFormulaDropDown(Sender: TObject);
begin
  if Sender is TComboBox then
    TComboBox(Sender).ItemWidth := CalcComboBoxItemWidth(TComboBox(Sender));
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if not FActivated then
  begin
    FActivated := true;
    Constraints.MinHeight := Toolbar.Height + ParamsPanel.Height;
    AutoSize := false;
    ReadFromIni;
    Calculate;
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CanClose then
    WriteToIni;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := APP_NAME;

  FRecentFilesManager := TMRUMenuManager.Create(self);
  FRecentFilesManager.MenuCaptionMask := '%0:d - %1:s';
  FRecentFilesManager.MaxRecent := MAX_MRU_FILE_COUNT;
  FRecentFilesManager.PopupMenu := RecentFilesPopup;
  FRecentFilesManager.OnRecentFile := @RecentFileHandler;
  FRecentFilesManager.IniFileName := GetIniFileName;
  FRecentFilesManager.IniSection := 'RecentFiles';

  FGenerator := TLiss3dGen.Create;

  FViewer := TLiss3dViewerFrame.Create(self);
  FViewer.Parent := ParamPanel;
  FViewer.Align := alClient;
  FViewer.BorderSpacing.Right := 4;
  FViewer.OnMouseMove := @ViewerMouseMove;

  // Add built-in formulas
  AddBuiltinFormulas;

  edFormulaX.DropDownCount := MAX_DROPDOWN_COUNT;
  edFormulaY.DropDownCount := MAX_DROPDOWN_COUNT;
  edFormulaZ.DropDownCount := MAX_DROPDOWN_COUNT;
  cbViewAngle.DropDownCount := MAX_DROPDOWN_COUNT;
  cbViewDirection.DropDownCount := MAX_DROPDOWN_COUNT;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FGenerator.Free;
end;

function TMainForm.GetIniFileName: String;
begin
  Result := Application.Location + 'lissajous.cfg';
end;

procedure TMainForm.LoadLissParams(ini: TCustomIniFile);
var
  section: String;
begin
  Assert(ini <> nil);

  inc(FViewerLock);

  ini.FormatSettings.DecimalSeparator := '.';
  ini.Options := ini.Options + [ifoFormatSettingsActive];

  section := 'Formula';
  edFormulaX.Text := ini.ReadString(section, 'x', '');
  edFormulaY.Text := ini.ReadString(section, 'y', '');
  edFormulaZ.Text := ini.ReadString(section, 'z', '');

  seCoeffA.Value := ini.ReadFloat(section, 'a', 0.0);
  seCoeffB.Value := ini.ReadFloat(section, 'b', 0.0);
  seCoeffC.Value := ini.ReadFloat(section, 'c', 0.0);
  seCoeffD.Value := ini.ReadFloat(section, 'd', 0.0);
  seStepCount.Value := ini.ReadInteger(section, 'StepCount', 0);
  seStepSize.Value := ini.ReadFloat(section, 'StepSize', 1.0);

  section := 'Rendering';
  cbBackgroundColor.ButtonColor := ini.ReadInteger(section, 'BackgroundColor', clBlack);
  seStickDiam.Value := ini.ReadFloat(section, 'StickDiameter', seStickDiam.Value);
  cbStickColor.ButtonColor := ini.ReadInteger(section, 'StickColor', Integer(cbStickColor.ButtonColor));
  seSymbolSize.Value := ini.ReadFloat(section, 'SymbolSize', seSymbolSize.Value);
  cbSymbolColor.ButtonColor := ini.ReadInteger(section, 'SymbolColor', Integer(cbSymbolColor.ButtonColor));
  cbViewAngle.ItemIndex := ini.ReadInteger(section, 'ViewAngle', cbViewAngle.ItemIndex);
  cbViewAngleChange(nil);
  cbViewDirection.ItemIndex := ini.ReadInteger(section, 'ViewDirection', 0);
  cbShowAxes.Checked := ini.ReadBool(section, 'ShowAxes', false);
  cbShowSticks.Checked := ini.ReadBool(section, 'ShowSticks', false);
  cbShowSymbols.Checked := ini.ReadBool(section, 'ShowSymbols', true);

  FViewer.CameraDistance := ini.ReadFloat(section, 'CameraDistance', FViewer.CameraDistance);
  FViewer.CameraRotX := ini.ReadFloat(section, 'RotationX', FViewer.CameraRotX);
  FViewer.CameraRotY := ini.ReadFloat(section, 'RotationY', FViewer.CameraRotY);
  FViewer.CameraRotZ := ini.ReadFloat(section, 'RotationZ', FViewer.CameraRotZ);

  FViewer.ShowAxes := cbShowAxes.Checked;

  dec(FViewerLock);
end;

procedure TMainForm.ReadFromIni;
var
  ini: TCustomIniFile;
  L, T, W, H: Integer;
  R: TRect;
  List: TStrings;
  s: String;
  ch: Char;
  i: Integer;
begin
  ini := TIniFile.Create(GetIniFileName);
  try
    T := ini.ReadInteger('MainForm', 'Top', Top);
    L := ini.ReadInteger('MainForm', 'Left', Left);
    W := ini.ReadInteger('MainForm', 'Width', Width);
    H := ini.ReadInteger('MainForm', 'Height', Height);
    R := Screen.WorkAreaRect;
    if W > R.Width then W := R.Width;
    if H > R.Height then H := R.Height;
    if L < R.Left then L := R.Left;
    if T < R.Top then T := R.Top;
    if L + W > R.Right then L := R.Right - W - GetSystemMetrics(SM_CXSIZEFRAME);
    if T + H > R.Bottom then T := R.Bottom - H - GetSystemMetrics(SM_CYCAPTION) - GetSystemMetrics(SM_CYSIZEFRAME);
    SetBounds(L, T, W, H);
    WindowState := wsNormal;
    Application.ProcessMessages;
    WindowState := TWindowState(ini.ReadInteger('MainForm', 'WindowState', 0));
    ParamPanel.Width := ini.ReadInteger('MainForm', 'ParamPanelWidth', ParamPanel.Width);

    inc(FViewerLock);

    edFormulaX.Items.Clear;
    edFormulaY.Items.Clear;
    edFormulaZ.Items.Clear;
    List := TStringList.Create;
    try
      ini.ReadSection('FormulaHistory', List);
      for i := 0 to List.Count-1 do
      begin
        s := List[i];
        ch := s[1];
        s := ini.ReadString('FormulaHistory', List[i], '');
        if s <> '' then
          case ch of
            'x': edFormulaX.Items.Add(s);
            'y': edFormulaY.Items.Add(s);
            'z': edFormulaZ.Items.Add(s);
          end;
      end;
      AddBuiltinFormulas;
    finally
      List.Free;
    end;

    LoadLissParams(ini);
    dec(FViewerLock);

    { Calculate    // <--- is executed by caller. }
  finally
    ini.Free;
  end;
end;

procedure TMainForm.RecentFileHandler(Sender: TObject;
  const AFileName: String);
begin
  LoadParamFile(AFileName);
end;

procedure TMainForm.SaveLissParams(ini: TCustomIniFile);
var
  section: String;
begin
  Assert(ini <> nil);

  ini.FormatSettings.DecimalSeparator := '.';
  ini.Options := ini.Options + [ifoFormatSettingsActive];

  section := 'Formula';
  ini.WriteString(section, 'x', edFormulaX.Text);
  ini.WriteString(section, 'y', edFormulaY.Text);
  ini.WriteString(section, 'z', edFormulaZ.Text);

  ini.WriteFloat(section, 'a', seCoeffA.Value);
  ini.WriteFloat(section, 'b', seCoeffB.Value);
  ini.WriteFloat(section, 'c', seCoeffC.Value);
  ini.WriteFloat(section, 'd', seCoeffD.Value);

  ini.WriteInteger(section, 'StepCount', seStepCount.Value);
  ini.WriteFloat(section, 'StepSize', seStepSize.Value);

  section := 'Rendering';
  ini.WriteInteger(section, 'BackgroundColor', cbBackgroundColor.ButtonColor);
  ini.WriteInteger(section, 'StickColor', cbStickColor.ButtonColor);
  ini.WriteFloat(section, 'StickDiameter', seStickDiam.Value);
  ini.WriteInteger(section, 'SymbolColor', cbSymbolColor.ButtonColor);
  ini.WriteFloat(section, 'SymbolSize', seSymbolSize.Value);
  ini.WriteInteger(section, 'ViewDirection', cbViewDirection.ItemIndex);
  ini.WriteInteger(section, 'ViewAngle', cbViewAngle.ItemIndex);
  ini.WriteBool(section, 'ShowAxes', cbShowAxes.Checked);
  ini.WriteBool(section, 'ShowSticks', cbShowSticks.Checked);
  ini.WriteBool(section, 'ShowSymbols', cbShowSymbols.Checked);
  ini.WriteString(section, 'Projection', GetEnumName(TypeInfo(TProjection), cbViewAngle.ItemIndex));
  ini.WriteFloat(section, 'CameraDistance', FViewer.CameraDistance);
  ini.WriteFloat(section, 'RotationX', FViewer.CameraRotX);
  ini.WriteFloat(section, 'RotationY', FViewer.CameraRotY);
  ini.WriteFloat(section, 'RotationZ', FViewer.CameraRotZ);
end;

procedure TMainForm.LoadParamFile(const AFileName: String);
var
  F: file of TLissFileRec;
  data: TLissFileRec;
  ini: TCustomIniFile;
begin
  try
    AssignFile(F, AFilename);
    Reset(F);
    Read(F, data);
  finally
    CloseFile(F);
  end;

  if not UnpackParams(data) then
  begin
    ini := TIniFile.Create(AFileName);
    try
      LoadLissParams(ini);
    finally
      ini.Free;
    end;
  end;

  Calculate;

  FRecentFilesManager.AddToRecent(AFileName);
  Caption := APP_NAME + ' - ' + AFileName;

  FFileName := AFileName;
end;

procedure TMainForm.seCameraDistanceChange(Sender: TObject);
begin
  if FViewerLock <> 0 then exit;
  FViewer.CameraDistance := seCameraDistance.Value;
  FViewer.InvalidateView;
end;


procedure TMainForm.seStickDiamChange(Sender: TObject);
begin
  FViewer.StickDiameter := seStickDiam.Value;
end;

procedure TMainForm.seSymbolSizeChange(Sender: TObject);
begin
  FViewer.SymbolSize := seSymbolSize.Value;
end;

procedure TMainForm.seXRotationChange(Sender: TObject);
begin
  if FViewerLock <> 0 then exit;
  FViewer.CameraRotX := seXRotation.Value;
  FViewer.InvalidateView;
end;

procedure TMainForm.seYRotationChange(Sender: TObject);
begin
  if FViewerLock <> 0 then exit;
  FViewer.CameraRotY := seYRotation.Value;
  FViewer.InvalidateView;
end;

procedure TMainForm.seZRotationChange(Sender: TObject);
begin
  if FViewerLock <> 0 then exit;
  FViewer.CameraRotZ := seZRotation.Value;
  FViewer.InvalidateView;
end;

function TMainForm.UnpackParams(AData: TLissFileRec): Boolean;
var
  s: string;
  i: byte;
  penNr: byte;
  penColor: Integer;
  smooth: boolean;
  grid: Boolean;
  incIdX: Integer;
  incSize: Single;
begin
  s := '';
  for i := 0 to 4 do
    s := s + chr(AData[i]);
  if (s <> SIGNATURE) then
  begin
    Result := false;
    exit;
  end;

  inc(FViewerLock);

  // Formula
  case AData[5] of
    1: begin
         edFormulaX.Text := BUILTIN_FORMULA1_X;
         edFormulaY.Text := BUILTIN_FORMULA1_Y;
         edFormulaZ.Text := BUILTIN_FORMULA1_Z;
       end;
    2: begin
         edFormulaX.Text := BUILTIN_FORMULA2_X;
         edFormulaY.Text := BUILTIN_FORMULA2_Y;
         edFormulaZ.Text := BUILTIN_FORMULA2_Z;
       end;
    3: begin
         edFormulaX.Text := BUILTIN_FORMULA3_X;
         edFormulaY.Text := BUILTIN_FORMULA3_Y;
         edFormulaZ.Text := BUILTIN_FORMULA3_Z;
       end;
  end;

  penNr := AData[6];

  // Symbol color
  pencolor := AData[7];
  if (penColor >= Low(ColorList)) and (penColor <= High(ColorList)) then
    cbSymbolColor.ButtonColor := ColorList[penColor]
  else
    cbSymbolColor.ButtonColor := penColor;

  // "smooth", meaning: show connections between symbols
  smooth := (AData[8] = 1);
  cbShowSticks.Checked := smooth;
  seStickDiam.Value := seSymbolSize.Value;
  cbStickColor.ButtonColor := cbSymbolColor.ButtonColor;

  grid := AData[9] = 1;

  // Step count
  seStepCount.Value := AData[10];

  // Step size
  incSize := PSingle(@AData[11])^;
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
  seCoeffA.Value := PSingle(@AData[12])^;
  seCoeffB.Value := PSingle(@AData[13])^;
  seCoeffC.Value := PSingle(@AData[14])^;
  seCoeffD.Value := PSingle(@AData[15])^;

  dec(FViewerLock);

  Result := true;
end;

procedure TMainForm.UpdateFormulaHistory;

  procedure UpdateHistory(ACombobox: TCombobox);
  var
    s: String;
    idx: Integer;
  begin
    s := ACombobox.Text;
    if s = '' then
      exit;

    idx := ACombobox.Items.IndexOf(s);
    if idx = 0 then
      exit;

    if idx = -1 then
    begin
      ACombobox.Items.Insert(0, s);
      while ACombobox.Items.Count > MAX_FORMULA_COUNT do
        ACombobox.Items.Delete(ACombobox.Items.Count-1);
    end else
      AComboBox.Items.Move(idx, 0);
    Acombobox.ItemIndex := 0;
  end;

begin
  UpdateHistory(edFormulaX);
  UpdateHistory(edFormulaY);
  UpdateHistory(edFormulaZ);
end;

procedure TMainForm.UpdateLissajousHandler(Sender: TObject);
begin
  Calculate;
end;

procedure TMainForm.UpdateLissParams;
begin
  FGenerator.SetCoeffs(seCoeffA.Value, seCoeffB.Value, seCoeffC.Value, seCoeffD.Value);
  FGenerator.SetFormulas(edFormulaX.Text, edFormulaY.Text, edFormulaZ.Text);
  FGenerator.StepCount := seStepCount.Value;
  FGenerator.StepSize := seStepSize.Value * pi/180;
end;

procedure TMainForm.ViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ([ssLeft, ssRight] * Shift <> []) then
    cbViewDirection.ItemIndex := 0;
end;

procedure TMainForm.WriteToIni;
var
  ini: TCustomIniFile;
  i: Integer;
begin
  ini := TIniFile.Create(GetIniFileName);
  try
    ini.WriteInteger('MainForm', 'Top', RestoredTop);
    ini.WriteInteger('MainForm', 'Left', RestoredLeft);
    ini.WriteInteger('MainForm', 'Width', RestoredWidth);
    ini.WriteInteger('MainForm', 'Height', RestoredHeight);
    ini.WriteInteger('MainForm', 'WindowState', Integer(WindowState));
    ini.WriteInteger('MainForm', 'ParamPanelWidth', ParamPanel.Width);

    for i:=0 to edFormulaX.Items.Count-1 do
      ini.WriteString('FormulaHistory', 'x' + IntToStr(i), edFormulaX.Items[i]);
    for i:=0 to edFormulaY.Items.Count-1 do
      ini.WriteString('FormulaHistory', 'y' + IntToStr(i), edFormulaY.Items[i]);
    for i:=0 to edFormulaZ.Items.Count-1 do
      ini.WriteString('FormulaHistory', 'z' + IntToStr(i), edFormulaZ.Items[i]);

    SaveLissParams(ini);
  finally
    ini.Free;
  end;
end;

end.

