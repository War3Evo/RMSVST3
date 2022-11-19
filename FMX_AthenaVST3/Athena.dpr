// "defaults" are stated as Delphi defaults and not defaults for VST3
// *** = changes from default
{$WRITEABLECONST OFF}         // default off
{$LONGSTRINGS ON}             // default on
{$TYPEDADDRESS OFF}           // default off
{$OPENSTRINGS ON}             // default on
{$EXTENDEDSYNTAX ON}          // default on
{$BOOLEVAL OFF}               // default off
{$VARSTRINGCHECKS OFF}        // default on ***
{$OPTIMIZATION ON}            // default on
// In the {$A8} or {$A+} state, fields in record types that are declared
// without the packed modifier and fields in class structures are aligned on quadword boundaries.
{$A+}                         // default {$A8} {$ALIGN 8} ???
{$STACKFRAMES OFF}            // default off
{$U-}                         // default {$U-}
{$RANGECHECKS OFF}            // default off
{$IOCHECKS OFF}               // default on ***
{$OVERFLOWCHECKS OFF}         // default off
{$DEBUGINFO OFF}              // default on ***
{$LOCALSYMBOLS OFF}           // default on ***
{$Y-}                         // default {$YD} {$DEFINITIONINFO ON} ***
{$ASSERTIONS OFF}             // default on ***

library Athena;

{$E vst3}

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  System.SysUtils,
  System.Classes,
  UCDataLayer in 'FrameworkCommon\UCDataLayer.pas',
  ULogger in 'FrameworkCommon\ULogger.pas',
  UVSTBase in 'FrameworkCommon\UVSTBase.pas',
  UCAudioProcessor in 'VST3SDK\UCAudioProcessor.pas',
  UCComponent in 'VST3SDK\UCComponent.pas',
  UCEditController in 'VST3SDK\UCEditController.pas',
  UCMidiMapping in 'VST3SDK\UCMidiMapping.pas',
  UCPluginFactory in 'VST3SDK\UCPluginFactory.pas',
  UCUnitInfo in 'VST3SDK\UCUnitInfo.pas',
  UVST3Controller in 'VST3SDK\UVST3Controller.pas',
  UVST3Processor in 'VST3SDK\UVST3Processor.pas',
  UVST3Utils in 'VST3SDK\UVST3Utils.pas',
  UVSTInstrument in 'VST3SDK\UVSTInstrument.pas',
  Vst3Base in 'VST3SDK\Vst3Base.pas',
  UCPlugView in 'VST3SDK\UCPlugView.pas',
  UAthenaVst in 'UAthenaVst.pas',
  UAthenaVstDSP in 'UAthenaVstDSP.pas',
  UAthenaVSTForm in 'UAthenaVSTForm.pas' {FormAthenaVST},
  UPianoKeyboard in 'PianoComponent_Install\UPianoKeyboard.pas';

{$R *.res}

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
