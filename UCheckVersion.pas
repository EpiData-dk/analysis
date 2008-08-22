unit UCheckVersion;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, InvokeRegistry, Rio, SOAPHTTPClient, StdCtrls,epdservices;

type
  TformCheckVersion = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbVersion: TLabel;
    lbRelease: TLabel;
    lbBuild: TLabel;
    HTTPRIO1: THTTPRIO;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  formCheckVersion: TformCheckVersion;

implementation

{$R *.dfm}

procedure TformCheckVersion.FormCreate(Sender: TObject);
var
  version, release, build: widestring;
  Iepd: IepdservicesPortType;
begin
  Iepd:=(httprio1 as IepdservicesPortType);
  Iepd.getEpiDataStatVersion(version,release,build);
  lbVersion.Caption:=version;
  lbRelease.Caption:=release;
  lbBuild.Caption:=build;
end;

end.

