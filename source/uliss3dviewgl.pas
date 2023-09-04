unit uLiss3dViewGL;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, Forms, Controls,
  gl, glu, OpenGLContext,
  uLiss3dTypes;

const
  VIEW_ANGLE = 45; //60.0;
  NEAR_CLIPDIST = 0.1;
  FAR_CLIPDIST = 100;

  CAMERA_DISTANCE = 5.0;

  SYMBOL_COLOR = clRed;
  SYMBOL_SIZE = 0.10;

type
  ToglVector2f = array[0..1] of GLFloat;
  ToglVector3f = array[0..2] of GLFloat;
  ToglVector4f = array[0..3] of GLFloat;

  TProjection = (oglPerspective, oglOrthographic);

  { TViewerFrame }

  TLiss3dViewerFrame = class(TFrame)
    OpenGLControl: TOpenGLControl;
    procedure OpenGLControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControlMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControlMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OpenGLControlPaint(Sender: TObject);
    procedure OpenGLControlResize(Sender: TObject);

  private
    FBackColor: TColor;
    FCameraAngle: Double;
    FCameraDistance: Double;
    FCameraRotX, FCameraRotY, FCameraRotZ: Double;
    FMouseX, FMouseY: Double;
    FShowAxes: Boolean;
    FShowSticks: Boolean;
    FShowSymbols: Boolean;
    FCylinder: PGLUQuadric;
    FStickColor: TColor;
    FStickDiameter: Double;
    FSphere: PGLUQuadric;
    FSymbolColor: TColor;
    FSymbolSize: Double;
    FProjection: TProjection;
    procedure SetBackColor(AValue: TColor);
    procedure SetPoints(const AValue: TPoint3dArray);
    procedure SetProjection(AValue: TProjection);
    procedure SetShowAxes(AValue: Boolean);
    procedure SetShowSticks(AValue: Boolean);
    procedure SetShowSymbols(AValue: Boolean);
    procedure SetStickColor(AValue: TColor);
    procedure SetStickDiameter(AValue: Double);
    procedure SetSymbolColor(AValue: TColor);
    procedure SetSymbolSize(AValue: Double);

  protected
    FInitDone: Boolean;
    FPoints: TPoint3dArray;
    procedure DrawAxes;
    procedure DrawCylinder(P1, P2: TPoint3d);
    procedure DrawScene;
    procedure DrawSphere(APoint: TPoint3d);
    procedure InitGL;
    procedure InitLights;
    procedure ToPerspective;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidateView;
    procedure ToBitmap(ABitmap: TCustomBitmap);

    property BackColor: TColor read FBackColor write SetBackColor;
    property CameraAngle: Double read FCameraAngle write FCameraAngle;
    property CameraDistance: Double read FCameraDistance write FCameraDistance;
    property CameraRotX: Double read FCameraRotX write FCameraRotX;
    property CameraRotY: Double read FCameraRotY write FCameraRotY;
    property CameraRotZ: Double read FCameraRotZ write FCameraRotZ;
    property Points: TPoint3dArray read FPoints write SetPoints;
    property Projection: TProjection read FProjection write SetProjection;
    property ShowAxes: Boolean read FShowAxes write SetShowAxes;
    property ShowSticks: Boolean read FShowSticks write SetShowSticks;
    property ShowSymbols: Boolean read FShowSymbols write SetShowSymbols;
    property StickColor: TColor read FStickColor write SetStickColor;
    property StickDiameter: Double read FStickDiameter write SetStickDiameter;
    property SymbolColor: TColor read FSymbolColor write SetSymbolColor;
    property SymbolSize: Double read FSymbolSize write SetSymbolSize;
  end;

implementation

{$R *.lfm}

uses
  GraphType;

procedure OpenGLToBitmap(ABitmap: TCustomBitmap);
const
  GL_BGRA = $80E1;
var
  viewport: array[0..3] of GLInt;
  rawImg: TRawImage;
begin
  if ABitmap = nil then
    raise Exception.Create('[OpenGLToBitmap] Bitmap must not be nil.');

  // Query size of the viewport
  glGetIntegerv(GL_VIEWPORT, @viewport);

  // Prepare a raw image
  rawImg.Init;
  rawImg.Description.Init_BPP32_B8G8R8A8_M1_BIO_TTB(viewport[2], viewport[3]);
  rawImg.Description.LineOrder := riloBottomToTop;
  rawImg.CreateData(false);

  // Query image data from OpenGL
  glReadPixels(0, 0, viewport[2], viewport[3], GL_BGRA, GL_UNSIGNED_BYTE, rawImg.Data);

  // Convert RawImage to Bitmap
  ABitmap.LoadFromRawImage(rawImg, false);

  // Cleanup
  rawImg.FreeData;
end;

procedure SetOpenGLColor(const AColor: TColor; Alpha: GLfloat = 1.0);
const
  f = 1.0/255;
begin
  glColor4f(Red(AColor)*f, Green(AColor)*f, Blue(AColor)*f, Alpha);
end;


{ TViewerFrame }

constructor TLiss3dViewerFrame.Create(AOwner: TComponent);
begin
  inherited;

  FCameraAngle := VIEW_ANGLE;
  FCameraDistance := CAMERA_DISTANCE;
  FStickColor := SYMBOL_COLOR;
  FStickDiameter := SYMBOL_SIZE * 0.5;
  FSymbolColor := SYMBOL_COLOR;
  FSymbolSize := SYMBOL_SIZE;

  FSphere := gluNewQuadric();
  gluQuadricDrawStyle(FSphere, GLU_FILL);

  FCylinder := gluNewQuadric();
  gluQuadricDrawStyle(FCylinder, GLU_FILL);
  gluQuadricNormals(FCylinder, GLU_SMOOTH);

  FShowAxes := false;
  FShowSymbols := true;
  FShowSticks := false;
end;

destructor TLiss3dViewerFrame.Destroy;
begin
  gluDeleteQuadric(FCylinder);
  gluDeleteQuadric(FSphere);
  inherited;
end;

procedure TLiss3dViewerFrame.DrawAxes;
const
  AXIS_LENGTH = 100;
var
  Origin: TPoint3d;
  hasBlend: Boolean;
  hasLighting: Boolean;
begin
  if not FShowAxes then
    exit;

  // Calculate position of origin in scaled coordinate system
  Origin.X := 0;
  Origin.Y := 0;
  Origin.Z := 0;

  glLineWidth(3);
  hasBlend := (glIsEnabled(GL_BLEND) = GL_TRUE);
  glEnable(GL_BLEND);  // for line smoothing
  hasLighting := (glIsEnabled(GL_LIGHTING) = GL_TRUE);
  glDisable(GL_LIGHTING);

  SetOpenGLColor(clRed);  // x axis --> red
  glBegin(GL_LINES);
    glVertex3f(Origin.x, Origin.y, Origin.z);
    glVertex3f(Origin.x + AXIS_LENGTH, Origin.y, Origin.z);
  glEnd;

  SetOpenGLColor(clGreen);  // y axis --> green
  glBegin(GL_LINES);
    glVertex3f(Origin.x, Origin.y, Origin.z);
    glVertex3f(Origin.x, Origin.y + AXIS_LENGTH, Origin.z);
  glEnd;

  SetOpenGLColor(clBlue);  // z axis --> blue
  glBegin(GL_LINES);
    glVertex3f(Origin.x, Origin.y, Origin.z);
    glVertex3f(Origin.x, Origin.y, Origin.z + AXIS_LENGTH);
  glEnd;

  if not hasBlend then
    glDisable(GL_BLEND);
  if hasLighting then
    glEnable(GL_LIGHTING);
end;

// Adapted from:
//   https://community.khronos.org/t/glucylinder-between-two-points/34447
procedure TLiss3dViewerFrame.DrawCylinder(P1, P2: TPoint3d);
const
  PRECISION = 9;
var
  radius: Double;
  v, vx, vy, vz, rx, ry, ax: Double;
  R2D: Double;
  tmp: TPoint3d;
begin
  radius := FStickDiameter * 0.5;

  if (P1.x = P2.x) and (P1.z = P2.z) and (P1.y < P2.y) then
  begin
    tmp := P1;
    P1 := P2;
    P2 := tmp;
  end;

  glPushMatrix;
  glTranslatef(P1.x, P1.y, P1.z);

  // Orientation vectors
  vx := P2.x - P1.x;  // component in x-direction
  vy := P2.y - P1.y;  // component in y-direction
  vz := P2.z - P1.z;  // component in z-direction

  v := sqrt(vx*vx + vy*vy + vz*vz);  // cylinder length

  // rotation vector, z x r
  rx := -vy*vz;
  ry := +vx*vz;
  ax := 0.0;
  if vz = 0 then
  begin
    ax := RadToDeg(arccos(vx/v));  // Rotation angle in x-y plane
    if vx <= 0 then ax := -ax;
  end else
  begin
    ax := RadToDeg(arccos(vz/v));  // Rotation angle
    if vz <= 0 then ax := -ax;
  end;

  if vz = 0 then
  begin
    glRotateD(90.0, 0, 1, 0.0);     // Rotate & align with x axis
    glRotateD(ax, -1.0, 0.0, 0.0);  // Rotate to point 2 in x-y plane
  end else
    glRotateD(ax, rx, ry, 0);       // Rotate about rotation vector

  // Draw the clyinder
  gluCylinder(FCylinder, radius, radius, v, PRECISION, PRECISION);

  glPopMatrix;
end;

procedure TLiss3dViewerFrame.DrawScene;
var
  i: Integer;
  P, P0: TPoint3d;
  hasColorMaterial: Boolean;
begin
  hasColorMaterial := (glIsEnabled(GL_COLOR_MATERIAL) = GL_TRUE);
  //glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE);
  glEnable(GL_COLOR_MATERIAL);

  for i:=0 to High(FPoints) do begin
    P := FPoints[i];
    if FShowSymbols then
    begin
      SetOpenGLColor(FSymbolColor);
      DrawSphere(P);
    end;
    if FShowSticks then
    begin
      if i > 0 then
      begin
        SetOpenGLColor(FStickColor);
        DrawCylinder(P0, P);
      end;
      P0 := P;
    end;
  end;

  if not hasColorMaterial then glDisable(GL_COLOR_MATERIAL);
end;

procedure TLiss3dViewerFrame.DrawSphere(APoint: TPoint3d);
begin
  glPushMatrix;
  glTranslatef(APoint.x, APoint.y, APoint.z);
  gluSphere(FSphere, FSymbolSize * 0.5, 18, 9);
  glPopMatrix;
end;

procedure TLiss3dViewerFrame.InitGL;
begin
  glShadeModel(GL_SMOOTH);                    // shading mathod: GL_SMOOTH or GL_FLAT
  glPixelStorei(GL_UNPACK_ALIGNMENT, 4);      // 4-byte pixel alignment

  // enable /disable features
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
  //glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_CULL_FACE);

  // track material ambient and diffuse from surface color, call it before glEnable(GL_COLOR_MATERIAL)
  //glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
  //glEnable(GL_COLOR_MATERIAL);

  glClearStencil(0);                          // clear stencil buffer
  glClearDepth(1.0);                          // 0 is near, 1 is far
  glDepthFunc(GL_LEQUAL);

  InitLights;

  // Create fonts
//  oglDestroyFont(FFont);
//  oglDestroyFont(FFixedFont);
//  FFont := oglCreateBitmapFont(wglGetCurrentDC, 'Arial', 24, [fsBold]);
//  FFixedFont := oglCreateBitmapFont(wglGetCurrentDC, 'Courier New', 14, []);

  // Load texture
//  FTexID := LoadTexture('../../src/earth2048.bmp', true);
end;

procedure TLiss3dViewerFrame.InitLights;
const
  AMBIENT: ToglVector4f  = (0.2, 0.2, 0.2, 1.0);    // ambient light
  DIFFUSE: ToglVector4f  = (0.7, 0.7, 0.7, 1.0);    // diffuse light
  SPECULAR: ToglVector4f = (1.0, 1.0, 1.0, 1.0);    // specular light
  LIGHT_POS: ToglVector4f = (0.0, 0.0, 20.0, 0.0);   // positional light
begin
  // set up light colors (ambient, diffuse, specular)
  glLightfv(GL_LIGHT0, GL_AMBIENT, @AMBIENT);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @DIFFUSE);
  glLightfv(GL_LIGHT0, GL_SPECULAR, @SPECULAR);

  // position the light
  glLightfv(GL_LIGHT0, GL_POSITION, @LIGHT_POS);

  // MUST enable each light source after configuration
  glEnable(GL_LIGHT0);
end;

procedure TLiss3dViewerFrame.InvalidateView;
begin
  FInitDone := false;
  OpenGLControl.Invalidate;
end;

procedure TLiss3dViewerFrame.OpenGLControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseX := X;
  FMouseY := Y;
end;

procedure TLiss3dViewerFrame.OpenGLControlMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if (ssLeft in Shift) then begin
    FCameraRotY := FCameraRotY + (x - FMouseX);
    FCameraRotX := FCameraRotX + (y - FMouseY);
    FMouseX := X;
    FMouseY := Y;
    OpenGLControl.Invalidate;
    if Assigned(OnMouseMove) then OnMouseMove(Sender, Shift, X, Y);
  end else
  if (ssRight in Shift) then begin
    FCameraRotZ := FCameraRotZ + (y - FMouseY) + (x - FMouseX);
    FMouseX := X;
    FMouseY := Y;
    OpenGLControl.Invalidate;
    if Assigned(OnMouseMove) then OnMouseMove(Sender, Shift, X, Y);
  end;
end;

procedure TLiss3dViewerFrame.OpenGLControlMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
const
  SPEED = 0.0005;
var
  f: Double;
begin
  if (ssCtrl in Shift) then
    f := -1.0
  else
    f := 1.0;
  if (ssShift in Shift) then
    f := f * 2;
  FCameraDistance := FCameraDistance * (1.0 + f * WheelDelta * SPEED);
  OpenGLControl.Invalidate;
end;

procedure TLiss3dViewerFrame.OpenGLControlPaint(Sender: TObject);
begin
  if not OpenGLControl.MakeCurrent then
    exit;

  if not FInitDone then begin
    InitGL;
    ToPerspective;
    FInitDone := true;
  end;

  // Clear buffer (background color)
  glClearColor(
    Red(FBackColor)/255,
    Green(FBackColor)/255,
    Blue(FBackColor)/255,
    1.0
  );
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);

  // Save the initial ModelView matrix before modifying ModelView matrix
  glPushMatrix();

  // Transform modelview matrix
  glTranslatef(0, 0, -FCameraDistance);
  glRotatef(FCameraRotX, 1, 0, 0);
  glRotatef(FCameraRotY, 0, 1, 0);
  glRotatef(FCameraRotZ, 0, 0, 1);

  // Draw axes and scene using transformed ModelView matrix
  DrawAxes;
  DrawScene;

  // Restore ModelView matrix
  glPopMatrix();

  OpenGLControl.SwapBuffers;
end;

procedure TLiss3dViewerFrame.OpenGLControlResize(Sender: TObject);
begin
  if FInitDone and OpenGLControl.MakeCurrent then
    ToPerspective;
end;

procedure TLiss3dViewerFrame.SetBackColor(AValue: TColor);
begin
  if AValue = FBackColor then
    exit;
  FBackColor := AValue;
  OpenGLControl.Invalidate;
end;

procedure TLiss3dViewerFrame.SetPoints(const AValue: TPoint3dArray);
begin
  SetLength(FPoints, Length(AValue));
  Move(AValue[0], FPoints[0], Length(AValue) * Sizeof(TPoint3d));
end;

procedure TLiss3dViewerFrame.SetProjection(AValue: TProjection);
begin
  if AValue = FProjection then
    exit;
  FProjection := AValue;
  InvalidateView;
end;

procedure TLiss3dViewerFrame.SetShowAxes(AValue: Boolean);
begin
  if AValue = FShowAxes then
    exit;
  FShowAxes := AValue;
  OpenGLControl.Invalidate;
end;

procedure TLiss3dViewerFrame.SetShowSticks(AValue: Boolean);
begin
  if AValue = FShowSticks then
    exit;
  FShowSticks := AValue;
  OpenGLControl.Invalidate;
end;

procedure TLiss3dViewerFrame.SetShowSymbols(AValue: Boolean);
begin
  if AValue = FShowSymbols then
    exit;
  FShowSymbols := AValue;
  OpenGLControl.Invalidate;
end;

procedure TLiss3dViewerFrame.SetStickColor(AValue: TColor);
begin
  if AValue = FStickColor then
    exit;
  FStickColor := AValue;
  OpenGLControl.Invalidate;
end;

procedure TLiss3dViewerFrame.SetStickDiameter(AValue: Double);
begin
  if AValue = FStickDiameter then
    exit;
  FStickDiameter := AValue;
  OpenGLControl.Invalidate;
end;

procedure TLiss3dViewerFrame.SetSymbolColor(AValue: TColor);
begin
  if AValue = FSymbolColor then
    exit;
  FSymbolColor := AValue;
  OpenGLControl.Invalidate;
end;

procedure TLiss3dViewerFrame.SetSymbolSize(AValue: Double);
begin
  if AValue = FSymbolSize then
    exit;
  FSymbolSize := AValue;
  OpenGLControl.Invalidate;
end;

procedure TLiss3dViewerFrame.ToBitmap(ABitmap: TCustomBitmap);
begin
  OpenGLControl.SwapBuffers;
  OpenGLToBitmap(ABitmap);
  OpenGLControl.SwapBuffers;
end;

procedure TLiss3dViewerFrame.ToPerspective;
var
  aspect: GLFloat;
  x1, x2, y1, y2: Double;

begin
  // Set viewport to be the entire window of the OpenGLControl
  glViewport(0, 0, OpenGLControl.Width, OpenGLControl.Height);

  // Set perspective viewing frustum
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  aspect := OpenGLControl.Width / OpenGLControl.Height;
  case FProjection of
    oglPerspective:
      begin
        //FCameraDistance := FCameraDistance * tan(DegToRad(VIEW_ANGLE)) / tan(DegToRad(FCameraAngle));
        gluPerspective(FCameraAngle, aspect, NEAR_CLIPDIST, FAR_CLIPDIST);
      end;
    oglOrthographic:
      begin
        x2 := 2.0 * FCameraDistance/CAMERA_DISTANCE;
        x1 := - x2;
        y1 := x1 / aspect;
        y2 := x2 / aspect;
        glOrtho(x1, x2, y1, y2, NEAR_CLIPDIST, FAR_CLIPDIST);
      end;
  end;

  // Switch to modelview matrix in order to set scene
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
end;

end.

