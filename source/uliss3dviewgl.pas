unit uLiss3dViewGL;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, Forms, Controls,
  gl, glu, OpenGLContext,
  uLiss3dTypes;

const
  VIEW_ANGLE = 45; //60.0;
  NEAR_CLIPDIST = 0.1;
  FAR_CLIPDIST = 100;

  CAMERA_DISTANCE = 5.0;

  SYMBOL_COLOR = clRed;
  SYMBOL_SIZE = 0.05;

type
  ToglVector2f = array[0..1] of GLFloat;
  ToglVector3f = array[0..2] of GLFloat;
  ToglVector4f = array[0..3] of GLFloat;

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
    FSphere: PGLUQuadric;
    FSymbolColor: TColor;
    FSymbolSize: Double;
    procedure SetBackColor(AValue: TColor);
    procedure SetPoints(const AValue: TPoint3dArray);
    procedure SetShowAxes(AValue: Boolean);
    procedure SetSymbolColor(AValue: TColor);

  protected
    FInitDone: Boolean;
    FPoints: TPoint3dArray;
    procedure DrawAxes;
    procedure DrawScene;
    procedure InitGL;
    procedure InitLights;
    procedure ToPerspective;

  public
    constructor Create(AOwner: TComponent); override;
    procedure InvalidateView;
    procedure ToBitmap(ABitmap: TCustomBitmap);

    property BackColor: TColor read FBackColor write SetBackColor;
    property CameraAngle: Double read FCameraAngle write FCameraAngle;
    property CameraDistance: Double read FCameraDistance write FCameraDistance;
    property CameraRotX: Double read FCameraRotX write FCameraRotX;
    property CameraRotY: Double read FCameraRotY write FCameraRotY;
    property CameraRotZ: Double read FCameraRotZ write FCameraRotZ;
    property Points: TPoint3dArray read FPoints write SetPoints;
    property ShowAxes: Boolean read FShowAxes write SetShowAxes;
    property SymbolColor: TColor read FSymbolColor write SetSymbolColor;
    property SymbolSize: Double read FSymbolSize write FSymbolSize;
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
  FSymbolColor := SYMBOL_COLOR;
  FSymbolSize := SYMBOL_SIZE;

  FSphere := gluNewQuadric();
  gluQuadricDrawStyle(FSphere, GLU_FILL);
end;

procedure TLiss3dViewerFrame.DrawAxes;
const
  EPS = 1e-3;
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

procedure TLiss3dViewerFrame.DrawScene;
var
  i: Integer;
  P: TPoint3d;
  hasColorMaterial: Boolean;
begin
  hasColorMaterial := (glIsEnabled(GL_COLOR_MATERIAL) = GL_TRUE);
  //glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE);
  glEnable(GL_COLOR_MATERIAL);

  SetOpenGLColor(FSymbolColor);
  for i:=0 to High(FPoints) do begin
    P := FPoints[i];
    glPushMatrix;
    glTranslatef(P.x, P.y, P.z);
    gluSphere(FSphere, FSymbolSize, 18, 9);
    glPopMatrix;
  end;

  if not hasColorMaterial then glDisable(GL_COLOR_MATERIAL);
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

  // clear buffer
  glClearColor(               // background color
    Red(FBackColor)/255,
    Green(FBackColor)/255,
    Blue(FBackColor)/255,
    1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);

  // save the initial ModelView matrix before modifying ModelView matrix
  glPushMatrix();

  // transform modelview matrix
  glTranslatef(0, 0, -FCameraDistance);
  glRotatef(FCameraRotX, 1, 0, 0);
  glRotatef(FCameraRotY, 0, 1, 0);
  glRotatef(FCameraRotZ, 0, 0, 1);

  DrawAxes;
  DrawScene;

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

procedure TLiss3dViewerFrame.SetShowAxes(AValue: Boolean);
begin
  if AValue = FShowAxes then
    exit;
  FShowAxes := AValue;
  OpenGLControl.Invalidate;
end;

procedure TLiss3dViewerFrame.SetSymbolColor(AValue: TColor);
begin
  if AValue = FSymbolColor then
    exit;
  FSymbolColor := AValue;
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
begin
  // Set viewport to be the entire window of the OpenGLControl
  glViewport(0, 0, OpenGLControl.Width, OpenGLControl.Height);

  // Set perspective viewing frustum
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  aspect := OpenGLControl.Width / OpenGLControl.Height;
  gluPerspective(VIEW_ANGLE, aspect, NEAR_CLIPDIST, FAR_CLIPDIST);

  // Switch to modelview matrix in order to set scene
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
end;

end.

