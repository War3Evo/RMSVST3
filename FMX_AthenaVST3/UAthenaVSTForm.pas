unit UAthenaVSTForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, UPianoKeyboard,
  FMX.StdCtrls, UAthenaVST, ULogger, FMX.Controls.Presentation;

type
  THostKeyEvent = procedure (key:integer;_on:boolean) of object;
  THostUpdateParameter = procedure (id:integer;value:double) of object;
  THostPrgmChange= procedure(prgm:integer) of object;

  TFormAthenaVST = class(TForm)
    FKeyboard: TRMCKeyboard;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    ScrollBar3: TScrollBar;
    Button1: TButton;
    procedure ScrollBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FkeyboardKeyEvent(Sender: TObject; key: Integer; _on,
      infinite: Boolean);
    procedure Button2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FScrollBars:array[0..2] of TScrollBar;
    //Fkeyboard:TRMCKeyboard;
    procedure CBOnKeyEvent(Sender: TObject; key: integer; _on, infinite: boolean);
  public
    { Public declarations }
    { property } HostKeyEvent: THostKeyEvent;
    { property } HostUpdateParameter:THostUpdateParameter;
    { property } HostPrgmChange:THostPrgmChange;
    procedure UpdateEditorParameter(index:integer;value: double);
    procedure SetProgram(prgm:integer);
    procedure SetKey(key:integer;_on:boolean);
  end;

var
  FormAthenaVST: TFormAthenaVST;

implementation

{$R *.fmx}

procedure TFormAthenaVST.FkeyboardKeyEvent(Sender: TObject; key: Integer; _on,
  infinite: Boolean);
begin
  WriteLog('FkeyboardKeyEvent Key = ' + key.ToString);
end;

procedure TFormAthenaVST.FormCreate(Sender: TObject);
VAR isb:integer;
begin
  //Fkeyboard:=TRMCKeyboard.Create(self);
  //Fkeyboard.Parent:=self;
  //Fkeyboard.Align:=Top;    //was alBottom
  //Fkeyboard.Height:=80;
  FScrollBars[0]:=ScrollBar1;
  FScrollBars[1]:=ScrollBar2;
  FScrollBars[2]:=ScrollBar3;
  Fkeyboard.OnKeyEvent:=CBOnKeyEvent;
end;

procedure TFormAthenaVST.FormShow(Sender: TObject);
begin
  //if FileExists('athena.json') then
    //SkAnimatedImage1.LoadFromFile('athena.json');
end;

procedure TFormAthenaVST.ScrollBar1Change(Sender: TObject);
VAR isb:integer;
    dbNum:double;
begin
  for isb:=0 to 2 do
  if Sender = FScrollBars[isb] then
    if assigned(HostUpdateParameter) then
      begin
        dbNum := FScrollBars[isb].Value;
        dbNum := dbNum / 100;
        HostUpdateParameter(ID_CUTOFF+isb,dbNum);
      end;
end;

procedure TFormAthenaVST.Button1Click(Sender: TObject);
begin
  if assigned(HostPrgmChange) then
    HostPrgmChange(1);
end;

procedure TFormAthenaVST.Button2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if assigned(HostKeyEvent) then
    begin
      Fkeyboard.SetKeyPressed(60,true);
      Fkeyboard.SetKeyPressed(64,true);
      Fkeyboard.SetKeyPressed(67,true);
      HostKeyEvent(60,true);
      HostKeyEvent(64,true);
      HostKeyEvent(67,true);
    end;
end;

procedure TFormAthenaVST.Button2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
//  Fkeyboard.PressKey(60,false);
//  Fkeyboard.PressKey(64,false);
//  Fkeyboard.PressKey(67,false);
  if assigned(HostKeyEvent) then
    begin
      Fkeyboard.SetKeyPressed(60,false);
      Fkeyboard.SetKeyPressed(64,false);
      Fkeyboard.SetKeyPressed(67,false);
      HostKeyEvent(60,false);
      HostKeyEvent(64,false);
      HostKeyEvent(67,false);
    end;
end;

procedure TFormAthenaVST.CBOnKeyEvent(Sender:TObject;key:integer;_on,infinite:boolean);
begin
  WriteLog('CBOnKeyEvent Key = ' + key.ToString);

  if assigned(HostKeyEvent) then
    HostKeyEvent(key,_on);
end;

procedure TFormAthenaVST.SetKey(key: integer; _on: boolean);
begin
  WriteLog('TFormAthenaVST.SetKey');
  Fkeyboard.SetKeyPressed(key,_on);
end;

procedure TFormAthenaVST.SetProgram(prgm: integer);
begin
  WriteLog('TFormAthenaVST.SetProgram:'+prgm.toString);
  //Label2.Caption:='Program:'+prgm.toString;
end;

procedure TFormAthenaVST.UpdateEditorParameter(index:integer;value: double);
VAR isb:integer;
    sNum:single;
begin
  WriteLog('TFormAthenaVST.UpdateEditorParameter(index:' + index.ToString + ' value:' + value.ToString);
  for isb:=0 to 2 do
    if index = ID_CUTOFF+isb then
      begin
        WriteLog('FScrollBars[' + isb.ToString + '].Value:=round(100*value)');
        sNum := round(100*value);
        WriteLog('sNum = ' + sNum.ToString);
        FScrollBars[isb].Value:=sNum;
        WriteLog('AFTER FScrollBars[isb].Value:=sNum;');
      end;
  WriteLog('TFormAthenaVST.UpdateEditorParameter END');
end;

end.
