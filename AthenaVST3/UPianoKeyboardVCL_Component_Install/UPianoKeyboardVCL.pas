{
Requires removed
vclactnband.dcp
vclx.dcp
xmlrtl.dcp
}

unit UPianoKeyboardVCL;

interface

uses   // removed Vcl.Dialogs, Vcl.Forms, Vcl.ExtCtrls,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.StdCtrls,
  Vcl.Direct2D, D2D1;


type
  TOnKeyEvent = procedure (Sender:TObject;key:integer;_on,infinite:boolean) of object;

  TRMCKeyboardVCL = class (TCustomControl)      //TGraphicControl
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  private
    FLastKey:integer;
    Foctaves:integer;
    FSelected:array of boolean;
    FDown:boolean;
    FOnKeyEvent:TOnKeyEvent;
    function LowerKey:integer;

    procedure SetOctaves(value:integer);
    function GetKey(X, Y: integer): integer;
    function GetBlackRect(octave,index: integer): TRect;
    function GetWhiteRect(octave,index: integer): TRect;
    procedure DrawBlackKey(D2DP: TDirect2DCanvas; octave,index: integer;selected:integer);
    procedure DrawWhiteKey(D2DP: TDirect2DCanvas; octave,index: integer;selected:integer);

    procedure SetKey(key: integer; _on: boolean;infinite:boolean=false);
    function xoffset(key: integer): integer;
  protected
    procedure Direct2DPaint(D2DP: TDirect2DCanvas); virtual;
  public
    constructor Create(owner:TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure SetKeyPressed(key:integer;_on:boolean);  // no sound
    procedure KeySoundOnly(key:integer;_on:boolean);       // sound only
    procedure PressKey(key:integer;_on:boolean);       // sound and highlight
  published
    property OnKeyEvent:TOnkeyEvent read FOnKeyEvent write FOnKeyEvent;
    property Octaves: integer read FOctaves write SetOctaves;
    property Anchors;
    property Align;
    property Enabled;
    property Visible;
  end;

procedure Register;

implementation

{ TMyKeyboard }

procedure Register;
begin
  RegisterComponents('RMC', [TRMCKeyboardVCL]); // Ruuds Midi Controls
end;

const blackkey:array[0..4] of integer = (1,3,6,8,10);
const whitekey:array[0..7] of integer = (0,2,4,5,7,9,11,12);

procedure TRMCKeyboardVCL.PressKey(key:integer;_on:boolean);             // Key pressed highlight and sound
begin
  SetKeyPressed(key,_on);
  SetKey(key,_on);
end;

procedure TRMCKeyboardVCL.KeySoundOnly(key:integer;_on:boolean);             // sound only
begin
  SetKey(key,_on);
end;

procedure TRMCKeyboardVCL.SetKey(key:integer;_on:boolean;infinite:boolean);
begin
  FSelected[key]:=_on;
  if assigned(FOnKeyEvent) then
    FOnKeyEvent(self,key+LowerKey,_on,infinite and _on);
  Invalidate;
end;

procedure TRMCKeyboardVCL.SetKeyPressed(key: integer; _on: boolean);      // highlight key pressed
begin
  dec(key,LowerKey);
  if (key>=0) and (key<=12*octaves) then
  begin
    FSelected[key]:=_on;
    Invalidate;
  end;
end;

procedure TRMCKeyboardVCL.MouseDown(Button: TMouseButton; Shift: TShiftState; X,  Y: Integer);
VAR key:integer;
begin
  key:=GetKey(X,Y);
  if key=-1 then exit;
  if Button = mbLeft then
  begin
    FDown:=true;
    SetKey(key,true);
  end;
  if Button = mbRight then
    SetKey(key,not FSelected[key],true);

end;

procedure TRMCKeyboardVCL.MouseMove(Shift: TShiftState; X, Y: Integer);
VAR key:integer;
begin
  key:=GetKey(X,Y);
  if (key<>FLastKey) then
  begin
    if FDown then SetKey(FLastKey,false);
    FLastKey:=key;
    if FDown then SetKey(FLastKey,true);
    Invalidate;
  end;
end;

procedure TRMCKeyboardVCL.MouseUp(Button: TMouseButton; Shift: TShiftState; X,  Y: Integer);
begin
  if not FDown then exit;
  if FLastKey<>-1 then
  begin
    FDown:=false;
    SetKey(FLastKey,false);
  end;
end;

procedure TRMCKeyboardVCL.CMMouseEnter(var Message: TMessage);
begin

end;

procedure TRMCKeyboardVCL.CMMouseLeave(var Message: TMessage);
begin
  MouseMove([],-100,-100);
end;

function inRect(x,y:integer;r:TRect):boolean;
begin
  with r do
    result:=(x>=Left) and (x<Right) and (y>=Top) and (y<Bottom);
end;

function TRMCKeyboardVCL.GetKey(X,Y:integer):integer;
VAR i,o:integer;
begin
  for o:=0 to Octaves-1 do
  begin
    for i:=0 to 4 do if inRect(X,Y,GetBlackRect(o,i)) then
    begin
      result:=blackkey[i]+12*o;
      exit;
    end;
  end;
  for o:=0 to Octaves-1 do
    for i:=0 to 7 do if inRect(X,Y,GetWhiteRect(o,i)) then
    begin
      result:=whitekey[i]+12*o;
      exit;
    end;
  if inRect(X,Y,GetWhiteRect(Octaves,0)) then result:=12*Octaves
  else result:=-1;
end;

function TRMCKeyboardVCL.GetBlackRect(octave,index:integer):TRect;
  const off: array[0..4] of integer = ( 17,49,95,126,156);
  function pixw(w,f1,f2:integer):integer; begin result:=round(w*f2/f1); end;
VAR w,x,scalew:integer;
begin
  scalew:= xoffset(7);
  x:= pixw(off[index],186,scalew)+xoffset(7*octave);
  w:= pixw(17,186,scalew);
  result:=Rect(x,0,x+w,round(height*0.63));
end;

function TRMCKeyboardVCL.xoffset(key:integer):integer;
begin
  result:=round(width*key/(7*octaves+1));
end;

function TRMCKeyboardVCL.GetWhiteRect(octave,index:integer):TRect;
VAR x,w:integer;
begin
  x:=xoffset(7*octave)+xoffset(index);
  w:=xoffset(index+1)-xoffset(index);
  result:=Rect(x,0,x+w,height);
end;

function TRMCKeyboardVCL.LowerKey: integer;
begin
  result:=12*(5-Octaves DIV 2);
end;

constructor TRMCKeyboardVCL.Create(owner: TComponent);
begin
  inherited;
  DoubleBuffered := true;
  Octaves:=3;
  Width:=600;
  Height:=90;
  FLastKey:=-1;
end;

destructor TRMCKeyboardVCL.Destroy;
begin
  inherited;
end;

procedure TRMCKeyboardVCL.DrawBlackKey(D2DP: TDirect2DCanvas; octave,index:integer;selected:integer);
begin
  D2DP.Pen.Color:=clBlack;
  D2DP.Pen.Style:=psSolid;
  D2DP.Pen.Width:=1;
  D2DP.Brush.Style:=bsSolid;
  case selected of
    0: D2DP.Brush.Color:=clBlack;
    1: D2DP.Brush.Color:=clLtGray;
    2: D2DP.Brush.Color:=clDkGray;
  end;
  with GetBlackRect(octave,index) do
    D2DP.Rectangle(Left,Top,Right,Bottom);
end;

procedure TRMCKeyboardVCL.DrawWhiteKey(D2DP: TDirect2DCanvas; octave,index:integer; selected:integer);
begin
  begin
    D2DP.Pen.Color:=clBlack;
    D2DP.Pen.Style:=psSolid;
    D2DP.Pen.Width:=1;
    D2DP.Brush.Style:=bsSolid;
    case selected of
      0: D2DP.Brush.Color:=clWhite;
      1: D2DP.Brush.Color:=clDkGray;
      2: D2DP.Brush.Color:=clLtGray;
    end;
    with GetWhiteRect(octave,index) do
      D2DP.Rectangle(Left,Top,Right,Bottom);
  end;
end;

procedure TRMCKeyboardVCL.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;


procedure TRMCKeyboardVCL.Paint;                                        //PAINT
var
  GT2DCanvas: TDirect2DCanvas;
begin
  if ((Self.Width > 1) and (Self.Height > 1)) then
  begin
    GT2DCanvas := TDirect2DCanvas.Create(Canvas, Bounds(0, 0, Width, Height));
    GT2DCanvas.RenderTarget.SetAntialiasMode(D2D1_ANTIALIAS_MODE_ALIASED);
    GT2DCanvas.BeginDraw;
    Direct2DPaint(GT2DCanvas);
    GT2DCanvas.EndDraw;
    GT2DCanvas.Free;
  end;
end;

procedure TRMCKeyboardVCL.Direct2DPaint(D2DP: TDirect2DCanvas);
  function getSelect(key:integer):integer;
  begin
    if Fselected[key] then result:=1
    else if key=FLastKey then result:=2
    else result:=0;
  end;
VAR i,o:integer;
begin
  for o:=0 to Octaves-1 do
  begin
    for i:=0 to 6 do DrawWhiteKey(D2DP,o,i,getSelect(o*12+whitekey[i]));
    if o=octaves-1 then DrawWhiteKey(D2DP,o,7,getSelect(octaves*12));
    for i:=0 to 4 do DrawBlackKey(D2DP,o,i,getSelect(o*12+blackkey[i]));
  end;
end;

procedure TRMCKeyboardVCL.SetOctaves(value: integer);
VAR i:integer;
begin
  Foctaves:=value;
  setLength(FSelected,12*FOctaves+1);
  for i:=0 to 12*FOctaves do FSelected[i]:=false;
  Invalidate;
end;

end.
