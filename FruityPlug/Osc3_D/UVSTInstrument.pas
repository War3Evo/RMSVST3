unit
    UVSTInstrument;


interface

uses
    Windows, Forms, SysUtils, Classes, Controls,UVSTBase,
    FP_Extra, FP_DelphiPlug, ActiveX, FP_PlugClass, FP_Def, ComCtrls;


var
   PlugInfo: TFruityPlugInfo = (
     SDKVersion  : CurrentSDKVersion;
     LongName    : 'ruuds pluginnetje';
     ShortName   : 'ruudje';
     Flags       : FPF_Type_FullGen;
     NumParams   : 0;
     DefPoly     : 0  // infinite
   );

type
    TVSTInstrument = class;
    TVST3Parameter  = record
                        id,steps:integer;
                        title,shorttitle,units:string;
                        min,max,defVal,value:double;
                        automate,isPreset:boolean; // visible in Host ?
                      end;
    TVST3ParameterArray = TArray<TVST3Parameter>;

    TRMSFruityPlug = class(TDelphiFruityPlug)
      FLongName,FShortName:AnsiString;
      FPlugin    : TVSTInstrument;

    public

      function Dispatcher(ID,Index,Value:IntPtr):IntPtr; override;
      procedure SaveRestoreState(const Stream:IStream;Save:LongBool); override;
      procedure GetName(Section,Index,Value:Integer;Name:PAnsiChar); override;
      function ProcessParam(ThisIndex,ThisValue,RECFlags:Integer):Integer; override;
      procedure Gen_Render(DestBuffer: PWAV32FS; var Length: integer); override;

      // internal
      constructor Create(SetTag:Integer; Host: TFruityPlugHost;pluginInfo:TVSTInstrumentInfo);

    end;
   TVSTInstrument = class(TVSTBase)
      Fparameters:TVST3ParameterArray;
      FFruityPlug:TRMSFruityPlug;
      function getEditorClass:TformClass;virtual;
      procedure UpdateHostParameter(id:integer;value:double);

      procedure OnEditOpen;virtual;
      procedure OnEditClose;virtual;
      procedure OnSamplerateChanged(samplerate:single);virtual;
      procedure Process32(samples,channels:integer;inputp, outputp: PPSingle);virtual;
      procedure UpdateProcessorParameter(id:integer;value:double);virtual;
      procedure OnInitialize;virtual;
      procedure UpdateEditorParameter(id:integer;value:double);virtual;
      procedure OnPresetChange(prgm:integer);virtual;
      procedure AddParameter(id:integer;title, shorttitle,units:string;min,max,val:double);
      procedure ResendParameters;
      function  EditorForm: TForm;
  private
    procedure ControllerInitialize(fruityPlug:TRMSFruityPlug);
   end;

function CreatePlugin(Host:TFruityPlugHost;Tag:TPluginTag;pluginInfo:TVSTInstrumentInfo): TFruityPlug;

implementation


const
     nMaxGrains  = 24;
     GrainLength = 512;  // samples per grain



// create an initialized plugin & return a pointer to the struct
function CreatePlugin(Host:TFruityPlugHost;Tag:TPluginTag;pluginInfo:TVSTInstrumentInfo): TFruityPlug;
begin
  Result := TRMSFruityPlug.Create(Tag, Host,pluginInfo);
end;

// create the object
constructor TRMSFruityPlug.Create(SetTag:Integer; Host: TFruityPlugHost;pluginInfo:TVSTInstrumentInfo);
var
   n : integer;
VAR FeditorFormClass:TFormClass;
begin
  inherited Create(SetTag, Host);
  FPlugin:=TVSTInstrument(pluginInfo.PluginDef.cl.Create);
  fPlugin.OnCreate(pluginInfo);
  PlugInfo.NumParams:=length(fPlugin.FParameters);
  FLongName:=pluginInfo.PluginDef.name+'('+pluginInfo.factoryDef.vendor+')';
  FShortName:=pluginInfo.PluginDef.name;
  Info := @PlugInfo;
//  PlugInfo.LongName:=@FLongName;
//  PlugInfo.ShortName:=@FShortName;

  // init

  FeditorFormClass := fPlugin.getEditorClass;
  if FeditorFormClass = NIL then FeditorFormClass:=PluginInfo.PluginDef.ecl;
  if FeditorFormClass = NIL then EditorForm:=NIL
  else EditorForm:=FeditorFormClass.Create(NIL);
(*
  with TSynthEditorForm(EditorForm) do
  begin
    FruityPlug := Self;
    for n := 0 to NumParamsConst-1 do
      if ParamCtrl[n] is TTrackBar then
        ParamValue[n] := TTrackBar(ParamCtrl[n]).Position;
    ProcessAllParams;
  end;  *)
end;



function TRMSFruityPlug.Dispatcher(ID, Index, Value: IntPtr): IntPtr;
begin
  Result := 0;

  case ID of
     // show the editor
     FPD_ShowEditor:
       with EditorForm do
       begin
         if Value = 0 then
         begin
           fPlugin.OnEditClose;
           Hide;  // I've swapped this line with the next
           ParentWindow := 0;
         end
         else
         begin
           ParentWindow := Value;
           Show;
           fPlugin.OnEditOpen;
         end;
         EditorHandle := Handle;
       end;

     FPD_SetSampleRate:
       begin
         fPlugin.OnSamplerateChanged(Value);
       end;
  end;
end;


// save/restore the state to/from a stream
procedure TRMSFruityPlug.SaveRestoreState;
begin
  if Save then
//    Stream.Write(@ParamValue, NumParamsConst * 4, nil)
  else
  begin
//    Stream.Read(@ParamValue, NumParamsConst * 4, nil);
//    ProcessAllParams;
  end;
end;

procedure TVSTInstrument.AddParameter(id: integer; title, shorttitle,  units: string; min, max, val: double);
begin

end;



procedure TVSTInstrument.UpdateEditorParameter(id: integer; value: double);
begin

end;

procedure TVSTInstrument.UpdateHostParameter(id: integer; value: double);
begin

end;

procedure TVSTInstrument.UpdateProcessorParameter(id: integer; value: double);
begin

end;


procedure TVSTInstrument.Process32(samples, channels: integer; inputp, outputp: PPSingle);
begin

end;

// params
function TRMSFruityPlug.ProcessParam(ThisIndex, ThisValue, RECFlags: integer): integer;
var
   o, i : integer;
   v    : single;
begin
(* . this needs some attention :}
  if ThisIndex < NumParamsConst then with TSynthEditorForm(EditorForm) do
  begin
    with TTrackBar(ParamCtrl[ThisIndex]) do
    begin
      if RECFlags and REC_FromMIDI <> 0 then
        ThisValue:=TranslateMIDI(ThisValue, Min, Max);

      if RECFlags and REC_UpdateValue <> 0 then
        ParamValue[ThisIndex] := ThisValue
      else if RECFlags and REC_GetValue <> 0 then
        ThisValue := ParamValue[ThisIndex];

      inc(ThisIndex);
      o := ThisIndex shr 2;
      i := (ThisIndex and 3) - 1;
      case i of
        // shape
        pOsc1Shape:  Osc[o].ShapeP := PlugHost.WaveTables[ThisValue];
        // level
        pOsc1Level:
          begin
            Osc[2].Level := ParamValue[pOsc3Level]*Div128;
            v := 1-Osc[2].Level;
            Osc[1].Level := v*ParamValue[pOsc2Level]*Div128;
            Osc[0].Level := v-Osc[1].Level;
            if RECFlags and REC_ShowHint <> 0 then
              ShowHintMsg_Percent(ThisValue, 128);
          end;
        // pitch
        pOsc1Coarse..pOsc1Fine:
          begin
            Osc[o].Pitch := ParamValue[pOsc1Coarse+o*4]*100+ParamValue[pOsc1Fine+o*4];
            if RECFlags and REC_ShowHint<>0 then
              ShowHintMsg_Pitch(ThisValue, i-pOsc1Coarse);
          end;
      end;

      if RECFlags and REC_UpdateControl<>0 then
        Position := ThisValue;
    end;
  end;
  Result := ThisValue;  *)
  Result:=0;
end;


procedure TVSTInstrument.ResendParameters;
begin

end;

procedure TRMSFruityPlug.GetName(Section, Index, Value: integer; Name: PAnsiChar);
begin
(*
  case Section of
    FPN_Param:
      StrPCopy(Name, GetLongHint(TSynthEditorForm(EditorForm).ParamCtrl[Index].Hint));
  end;  *)
end;

procedure TVSTInstrument.ControllerInitialize;
begin
  FFruityPlug:=FruityPlug;
  OnInitialize;
end;

procedure TVSTInstrument.OnEditClose;
begin
// virtual
end;

procedure TVSTInstrument.OnEditOpen;
begin
// virtual
end;

procedure TVSTInstrument.OnInitialize;
begin

end;

procedure TVSTInstrument.OnPresetChange(prgm: integer);
begin

end;

procedure TVSTInstrument.OnSamplerateChanged(samplerate: single);
begin
// virtual
end;

procedure TRMSFruityPlug.Gen_Render(DestBuffer: PWAV32FS; var Length: integer);
begin
  // if there is nothing to do then...
  Length:=0;
  // else Process and Deinterlase
end;

function TVSTInstrument.EditorForm: TForm;
begin
  result:=FFruityPlug.EditorForm;
end;

function TVSTInstrument.getEditorClass: TformClass;
begin
  result:=NIL;
end;

end.




