// ************************************************************************ //
// The types declared in this file were generated from data read from the
// WSDL File described below:
// WSDL     : http://www.epidata.dk/ws/epdservices.php?wsdl
// Encoding : ISO-8859-1
// Version  : 1.0
// (14-06-2005 01:29:58 - 1.33.2.5)
// ************************************************************************ //

unit epdservices;

interface

uses InvokeRegistry, SOAPHTTPClient, Types, XSBuiltIns;

type

  // ************************************************************************ //
  // The following types, referred to in the WSDL document are not being represented
  // in this file. They are either aliases[@] of other types represented or were referred
  // to but never[!] declared in the document. The types from the latter category
  // typically map to predefined/known XML or Borland types; however, they could also 
  // indicate incorrect WSDL documents that failed to declare or import a schema type.
  // ************************************************************************ //
  // !:string          - "http://www.w3.org/2001/XMLSchema"



  // ************************************************************************ //
  // Namespace : urn:getEpiDataStatVersion
  // soapAction: urn:%operationName%#get
  // transport : http://schemas.xmlsoap.org/soap/http
  // style     : rpc
  // binding   : IepdservicesBinding
  // service   : Iepdservices
  // port      : IepdservicesPort
  // URL       : http://www.epidata.dk/ws/epdservices.php
  // ************************************************************************ //
  IepdservicesPortType = interface(IInvokable)
  ['{EDA665AB-9D22-51B6-8D73-75890FC39877}']
    procedure getEpiDataStatVersion(out version: WideString; out release: WideString; out build: WideString; out info: WideString); stdcall;
    procedure getEpiDataVersion(out version: WideString; out build: WideString); stdcall;
  end;

function GetIepdservicesPortType(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): IepdservicesPortType;


implementation

function GetIepdservicesPortType(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): IepdservicesPortType;
const
  defWSDL = 'http://www.epidata.dk/ws/epdservices.php?wsdl';
  defURL  = 'http://www.epidata.dk/ws/epdservices.php';
  defSvc  = 'Iepdservices';
  defPrt  = 'IepdservicesPort';
var
  RIO: THTTPRIO;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := defWSDL
    else
      Addr := defURL;
  end;
  if HTTPRIO = nil then
    RIO := THTTPRIO.Create(nil)
  else
    RIO := HTTPRIO;
  try
    Result := (RIO as IepdservicesPortType);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := defSvc;
      RIO.Port := defPrt;
    end else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;


initialization
  InvRegistry.RegisterInterface(TypeInfo(IepdservicesPortType), 'urn:getEpiDataStatVersion', 'ISO-8859-1');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(IepdservicesPortType), 'urn:%operationName%#get');

end. 
