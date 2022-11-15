{
  System.Classes.TThread.GetTickCount is cross platform
}

unit ULogger;    // renamed from UCodeSiteLogger

interface

procedure WriteLog(s:string);

implementation

//uses Windows, System.IOUtils, Classes, SysUtils, CodeSiteLogging;   --> VCL
uses System.SysUtils, System.IOUtils, System.Classes, CodeSiteLogging;

// Comment this below out if you don't have CodeSiteLogging:
VAR FlastCheck:Int64;
    FLastResult:boolean;
function Enabled:boolean;
VAR fname:string;
    sl:TStringlist;
    p:integer;
begin
  if TThread.GetTickCount>FLastCheck+5000 then
  begin
    FLastCheck:=TThread.GetTickCount;
    FLastResult:=true; // default = on
    fname:=TPath.GetDocumentsPath+'\My CodeSite Files\Logging.inf';
    sl:=TStringlist.Create;
    try
      sl.LoadFromFile(fname);
      p:=Pos('LOG=',UpperCase(sl[0]));
      if p=1 then FLastResult:=sl[0][5]='1';
    except
    end;
    sl.Free;
  end;
  result:=FLastResult;
end;
// Comment this above out if you don't have CodeSiteLogging


procedure WriteLog(s:string);
begin
  //  if not Enabled then exit;
  // Comment the next line out if you don't have CodeSiteLogging:
  CodeSite.Send(s);
end;

begin
  // Comment the next line out if you don't have CodeSiteLogging:
  FLastResult:=true;
end.
