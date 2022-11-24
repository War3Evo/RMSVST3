{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library AthenaVST3;

{$E vst3}

uses
  UCPluginFactory in 'VST3SDK\UCPluginFactory.pas',
  UVST3Utils in 'VST3SDK\UVST3Utils.pas',
  UCEditController in 'VST3SDK\UCEditController.pas',
  UCAudioProcessor in 'VST3SDK\UCAudioProcessor.pas',
  UCComponent in 'VST3SDK\UCComponent.pas',
  UCPlugView in 'VST3SDK\UCPlugView.pas',
  UCMidiMapping in 'VST3SDK\UCMidiMapping.pas',
  UVSTInstrument in 'VST3SDK\UVSTInstrument.pas',
  UCUnitInfo in 'VST3SDK\UCUnitInfo.pas',
  UCDataLayer in 'FrameworkCommon\UCDataLayer.pas',
  UVST3Processor in 'VST3SDK\UVST3Processor.pas',
  UVST3Controller in 'VST3SDK\UVST3Controller.pas',
  UVSTBase in 'FrameworkCommon\UVSTBase.pas',
  Vst3Base in 'VST3SDK\Vst3Base.pas',
  ULogger in 'FrameworkCommon\ULogger.pas',
  UAthenaVst in 'UAthenaVst.pas',
  UAthenaVstDSP in 'UAthenaVstDSP.pas',
  UAthenaVSTForm in 'UAthenaVSTForm.pas' {FormAthenaVST},
  UPianoKeyboardVCL in 'UPianoKeyboardVCL_Component_Install\UPianoKeyboardVCL.pas';

function InitDLL:boolean; cdecl; export;
begin
 Result := true;
end;

function ExitDLL:boolean; cdecl; export;
begin
 Result := true;
end;

function GetPluginFactory: pointer;stdcall; export;
begin
  result:=CreatePlugin(GetVSTInstrumentInfo);
end;


exports
  InitDLL name 'InitDLL',
  ExitDLL name 'ExitDLL',
  GetPluginFactory name 'GetPluginFactory';

begin
end.

