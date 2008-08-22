object formCheckVersion: TformCheckVersion
  Left = 604
  Top = 362
  BorderStyle = bsDialog
  Caption = 'Check version'
  ClientHeight = 174
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 72
    Top = 32
    Width = 155
    Height = 13
    Caption = 'Newest version of EpiDataStat is'
  end
  object Label2: TLabel
    Left = 88
    Top = 56
    Width = 35
    Height = 13
    Caption = 'Version'
  end
  object Label3: TLabel
    Left = 88
    Top = 80
    Width = 39
    Height = 13
    Caption = 'Release'
  end
  object Label4: TLabel
    Left = 88
    Top = 104
    Width = 23
    Height = 13
    Caption = 'Build'
  end
  object lbVersion: TLabel
    Left = 144
    Top = 56
    Width = 43
    Height = 13
    Caption = 'lbVersion'
  end
  object lbRelease: TLabel
    Left = 144
    Top = 80
    Width = 47
    Height = 13
    Caption = 'lbRelease'
  end
  object lbBuild: TLabel
    Left = 144
    Top = 104
    Width = 31
    Height = 13
    Caption = 'lbBuild'
  end
  object Button1: TButton
    Left = 136
    Top = 135
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object HTTPRIO1: THTTPRIO
    WSDLLocation = 'http://www.epidata.dk/ws/epdservices.php?wsdl'
    Service = 'Iepdservices'
    Port = 'IepdservicesPort'
    HTTPWebNode.Agent = 'Borland SOAP 1.2'
    HTTPWebNode.UseUTF8InHeader = False
    HTTPWebNode.InvokeOptions = [soIgnoreInvalidCerts, soAutoCheckAccessPointViaUDDI]
    Converter.Options = [soSendMultiRefObj, soTryAllSchema, soRootRefNodesToBody, soCacheMimeResponse, soUTF8EncodeXML]
    Left = 296
    Top = 24
  end
end
