{+-----------------------------------------------------------------------------+
 | Class:       TmwCustomHighlighter
 | Created:     07.98 - 10.98
 | Last change: 1999-11-14
 | Author:      Martin Waldenburg
 | Description: Parent class for all highlighters.
 | Version:     0.71 (for version history see version.rtf)
 | Copyright (c) 1998 Martin Waldenburg
 | All rights reserved.
 |
 | Thanks to: Primoz Gabrijelcic, Michael Trier, James Jacobson,
 |            Cyrille de Brebisson, Andy Jeffries
 |
 | LICENCE CONDITIONS
 |
 | USE OF THE ENCLOSED SOFTWARE
 | INDICATES YOUR ASSENT TO THE
 | FOLLOWING LICENCE CONDITIONS.
 |
 |
 |
 | These Licence Conditions are exlusively
 | governed by the Law and Rules of the
 | Federal Republic of Germany.
 |
 | Redistribution and use in source and binary form, with or without
 | modification, are permitted provided that the following conditions
 | are met:
 |
 | 1. Redistributions of source code must retain the above copyright
 |    notice, this list of conditions and the following disclaimer.
 |    If the source is modified, the complete original and unmodified
 |    source code has to distributed with the modified version.
 |
 | 2. Redistributions in binary form must reproduce the above
 |    copyright notice, these licence conditions and the disclaimer
 |    found at the end of this licence agreement in the documentation
 |    and/or other materials provided with the distribution.
 |
 | 3. Software using this code must contain a visible line of credit.
 |
 | 4. If my code is used in a "for profit" product, you have to donate
 |    to a registered charity in an amount that you feel is fair.
 |    You may use it in as many of your products as you like.
 |    Proof of this donation must be provided to the author of
 |    this software.
 |
 | 5. If you for some reasons don't want to give public credit to the
 |    author, you have to donate three times the price of your software
 |    product, or any other product including this component in any way,
 |    but no more than $500 US and not less than $200 US, or the
 |    equivalent thereof in other currency, to a registered charity.
 |    You have to do this for every of your products, which uses this
 |    code separately.
 |    Proof of this donations must be provided to the author of
 |    this software.
 |
 |
 | DISCLAIMER:
 |
 | THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS'.
 |
 | ALL EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 | THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 | PARTICULAR PURPOSE ARE DISCLAIMED.
 |
 | IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 | INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 | (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 | OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 | INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 | WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 | NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 | THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 |
 |  Martin.Waldenburg@T-Online.de
 |
 +----------------------------------------------------------------------------+}

{$I MWEDIT.INC}

unit mwHighlighter;

interface

uses
  Windows, SysUtils, Classes, Graphics, Registry, mwSupportClasses;

{$DEFINE _Gp_MustEnhanceRegistry}
{$IFDEF MWE_COMPILER_4_UP}
  {$UNDEF _Gp_MustEnhanceRegistry}
{$ENDIF}
type
  TBetterRegistry = class(TRegistry)
  {$IFDEF _Gp_MustEnhanceRegistry}
    function OpenKeyReadOnly(const Key: string): Boolean;
  {$ENDIF}
  end;

  TmwHighLightAttributes = Class(TPersistent)
  private
    fBackground: TColor;
    fForeground: TColor;
    fStyle: TFontStyles;
    fOnChange: TNotifyEvent;
    fName: string;
    procedure SetBackground(Value: TColor);
    procedure SetForeground(Value: TColor);
    procedure SetStyle(Value: TFontStyles);
    function GetStyleFromInt: integer;
    procedure SetStyleFromInt(const Value: integer);
  protected
  public
    procedure Assign(Source: TPersistent); override;
    function LoadFromBorlandRegistry(rootKey: HKEY;
               attrKey, attrName: string; oldStyle: boolean): boolean; virtual;
    function LoadFromRegistry(Reg: TBetterRegistry): boolean;
    function SaveToRegistry(Reg: TBetterRegistry): boolean;

    property IntegerStyle: integer read GetStyleFromInt write SetStyleFromInt;
    property Name: string read fName;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  published
    constructor Create(attribName: string);
    property Background: TColor read fBackground write SetBackground;
    property Foreground: TColor read fForeground write SetForeground;
    property Style: TFontStyles read fStyle write SetStyle;
  end;

  TIdentChars = set of char;

  THighlighterCapabilities = (
    hcUserSettings, // supports Enum/UseUserSettings
    hcRegistry,     // supports LoadFrom/SaveToRegistry
    hcExportable    // supports Exporters
  );

  THighlighterCapability = set of THighlighterCapabilities;

  TTokenEvent = procedure(Sender: TObject; TokenKind: integer;
    TokenText: String; LineNo: Integer) of Object;

  TmwCustomHighLighter = Class(TComponent)
  private
    fAttributes: TStringList;
    fAttrChangeHooks: TmwNotifyEventChain;
    fOnToken: TTokenEvent;
    fExporter: TComponent;
  protected
    fDefaultFilter: string;
    function GetIdentChars: TIdentChars; virtual;
    function GetLanguageName: string; virtual; abstract;

    procedure AddAttribute(AAttrib: TmwHighLightAttributes);
    procedure DefHighlightChange(Sender: TObject);
    function GetAttribCount: integer; virtual;
    function GetAttribute(idx: integer): TmwHighLightAttributes; virtual;
    procedure SetAttributesOnChange(AEvent: TNotifyEvent);
    function GetCapability: THighlighterCapability; virtual;
    function GetDefaultFilter: string; virtual;
    procedure SetDefaultFilter(Value: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExportNext;virtual; abstract;
    function GetEol: Boolean; virtual; abstract;
    function GetRange: Pointer; virtual; abstract;
    function GetToken: String; virtual; abstract;
    function GetTokenAttribute: TmwHighLightAttributes; virtual; abstract;
    function GetTokenKind: integer; virtual; abstract;
    function GetTokenPos: Integer; virtual; abstract;
    procedure Next; virtual; abstract;
    procedure NextToEol;
    procedure ScanAllLineTokens(const Value: string; LineNumber: integer);
    procedure SetLine(NewValue: String; LineNumber:Integer); virtual; abstract;
    procedure SetLineForExport(NewValue: String);virtual; abstract;
    procedure SetRange(Value: Pointer); virtual; abstract;
    procedure ReSetRange; virtual; abstract;
    function UseUserSettings(settingIndex: integer): boolean; virtual;

    procedure EnumUserSettings(settings: TStrings); virtual;

    function LoadFromRegistry(RootKey: HKEY; Key: string): boolean; virtual;
    function SaveToRegistry(RootKey: HKEY; Key: string): boolean; virtual;
    procedure HookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
    procedure UnhookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
    property IdentChars: TIdentChars read GetIdentChars;
    property LanguageName: string read GetLanguageName;

    property AttrCount: integer read GetAttribCount;
    property Attribute[idx: integer]: TmwHighLightAttributes read GetAttribute;
    property Capability: THighlighterCapability read GetCapability;
    property Exporter:TComponent read fExporter write fExporter;
  published
    property DefaultFilter: string read GetDefaultFilter write SetDefaultFilter;
    property OnToken: TTokenEvent read fOnToken write fOnToken;
  end;

implementation

{$IFDEF _Gp_MustEnhanceRegistry}
  function IsRelative(const Value: string): Boolean;
  begin
    Result := not ((Value <> '') and (Value[1] = '\'));
  end;

  function TBetterRegistry.OpenKeyReadOnly(const Key: string): Boolean;
  var
    TempKey: HKey;
    S: string;
    Relative: Boolean;
  begin
    S := Key;
    Relative := IsRelative(S);

    if not Relative then Delete(S, 1, 1);
    TempKey := 0;
    Result := RegOpenKeyEx(GetBaseKey(Relative), PChar(S), 0,
        KEY_READ, TempKey) = ERROR_SUCCESS;
    if Result then
    begin
      if (CurrentKey <> 0) and Relative then S := CurrentPath + '\' + S;
      ChangeKey(TempKey, S);
    end;
  end; { TBetterRegistry.OpenKeyReadOnly }
{$ENDIF _Gp_MustEnhanceRegistry}

{ TmwHighLightAttributes }

procedure TmwHighLightAttributes.Assign(Source: TPersistent);
begin
  if Source is TmwHighLightAttributes then begin
    fBackground    := (Source as TmwHighLightAttributes).fBackground;
    fForeground    := (Source as TmwHighLightAttributes).fForeground;
    fStyle         := (Source as TmwHighLightAttributes).fStyle;
    fName          := (Source as TmwHighLightAttributes).fName;
  end
  else inherited Assign(Source);
end;

constructor TmwHighLightAttributes.Create(attribName: string);
begin
  inherited Create;
  Background := clWindow;
  Foreground := clWindowText;
  fName := attribName;
end;

function TmwHighLightAttributes.LoadFromBorlandRegistry(rootKey: HKEY;
  attrKey, attrName: string; oldStyle: boolean): boolean;
  // How the highlighting information is stored:
  // Delphi 1.0:
  //   I don't know and I don't care.
  // Delphi 2.0 & 3.0:
  //   In the registry branch HKCU\Software\Borland\Delphi\x.0\Highlight
  //   where x=2 or x=3.
  //   Each entry is one string value, encoded as
  //     <foreground RGB>,<background RGB>,<font style>,<default fg>,<default Background>,<fg index>,<Background index>
  //   Example:
  //     0,16777215,BI,0,1,0,15
  //     foreground color (RGB): 0
  //     background color (RGB): 16777215 ($FFFFFF)
  //     font style: BI (bold italic), possible flags: B(old), I(talic), U(nderline)
  //     default foreground: no, specified color will be used (black (0) is used when this flag is 1)
  //     default background: yes, white ($FFFFFF, 15) will be used for background
  //     foreground index: 0 (foreground index (Pal16), corresponds to foreground RGB color)
  //     background index: 15 (background index (Pal16), corresponds to background RGB color)
  // Delphi 4.0 & 5.0:
  //   In the registry branch HKCU\Software\Borland\Delphi\4.0\Editor\Highlight.
  //   Each entry is subkey containing several values:
  //     Foreground Color: foreground index (Pal16), 0..15 (dword)
  //     Background Color: background index (Pal16), 0..15 (dword)
  //     Bold: fsBold yes/no, 0/True (string)
  //     Italic: fsItalic yes/no, 0/True (string)
  //     Underline: fsUnderline yes/no, 0/True (string)
  //     Default Foreground: use default foreground (clBlack) yes/no, False/-1 (string)
  //     Default Background: use default backround (clWhite) yes/no, False/-1 (string)
const
  Pal16: array [0..15] of TColor = (clBlack, clMaroon, clGreen, clOlive,
          clNavy, clPurple, clTeal, clLtGray, clDkGray, clRed, clLime,
          clYellow, clBlue, clFuchsia, clAqua, clWhite);

  function LoadOldStyle(rootKey: HKEY; attrKey, attrName: string): boolean;
  var
    descript : string;
    fgColRGB : string;
    bgColRGB : string;
    fontStyle: string;
    fgDefault: string;
    bgDefault: string;
    fgIndex16: string;
    bgIndex16: string;
    reg      : TBetterRegistry;

    function Get(var name: string): string;
    var
      p: integer;
    begin
      p := Pos(',',name);
      if p = 0 then p := Length(name)+1;
      Result := Copy(name,1,p-1);
      name := Copy(name,p+1,Length(name)-p);
    end; { Get }

  begin { LoadOldStyle }
    Result := false;
    try
      reg := TBetterRegistry.Create;
      reg.RootKey := rootKey;
      try
        with reg do begin
          if OpenKeyReadOnly(attrKey) then begin
            try
              if ValueExists(attrName) then begin
                descript := ReadString(attrName);
                fgColRGB  := Get(descript);
                bgColRGB  := Get(descript);
                fontStyle := Get(descript);
                fgDefault := Get(descript);
                bgDefault := Get(descript);
                fgIndex16 := Get(descript);
                bgIndex16 := Get(descript);
                if bgDefault = '1'
                  then Background := clWindow
                  else Background := Pal16[StrToInt(bgIndex16)];
                if fgDefault = '1'
                  then Foreground := clWindowText
                  else Foreground := Pal16[StrToInt(fgIndex16)];
                Style := [];
                if Pos('B',fontStyle) > 0 then Style := Style + [fsBold];
                if Pos('I',fontStyle) > 0 then Style := Style + [fsItalic];
                if Pos('U',fontStyle) > 0 then Style := Style + [fsUnderline];
                Result := true;
              end;
            finally CloseKey; end;
          end; // if
        end; // with
      finally reg.Free; end;
    except end;
  end; { LoadOldStyle }

  function LoadNewStyle(rootKey: HKEY; attrKey, attrName: string): boolean;
  var
    fgIndex16    : DWORD;
    bgIndex16    : DWORD;
    fontBold     : string;
    fontItalic   : string;
    fontUnderline: string;
    fgDefault    : string;
    bgDefault    : string;
    reg          : TBetterRegistry;

    function IsTrue(value: string): boolean;
    begin
      Result := not ((UpperCase(value) = 'FALSE') or (value = '0')); 
    end; { IsTrue }

  begin
    Result := false;
    try
      reg := TBetterRegistry.Create;
      reg.RootKey := rootKey;
      try
        with reg do begin
          if OpenKeyReadOnly(attrKey+'\'+attrName) then begin
            try
              if ValueExists('Foreground Color')
                then fgIndex16 := ReadInteger('Foreground Color')
                else Exit;
              if ValueExists('Background Color')
                then bgIndex16 := ReadInteger('Background Color')
                else Exit;
              if ValueExists('Bold')
                then fontBold := ReadString('Bold')
                else Exit;
              if ValueExists('Italic')
                then fontItalic := ReadString('Italic')
                else Exit;
              if ValueExists('Underline')
                then fontUnderline := ReadString('Underline')
                else Exit;
              if ValueExists('Default Foreground')
                then fgDefault := ReadString('Default Foreground')
                else Exit;
              if ValueExists('Default Background')
                then bgDefault := ReadString('Default Background')
                else Exit;
              if IsTrue(bgDefault)
                then Background := clWindow
                else Background := Pal16[bgIndex16];
              if IsTrue(fgDefault)
                then Foreground := clWindowText
                else Foreground := Pal16[fgIndex16];
              Style := [];
              if IsTrue(fontBold) then Style := Style + [fsBold];
              if IsTrue(fontItalic) then Style := Style + [fsItalic];
              if IsTrue(fontUnderline) then Style := Style + [fsUnderline];
              Result := true;
            finally CloseKey; end;
          end; // if
        end; // with
      finally reg.Free; end;
    except end;
  end; { LoadNewStyle }

begin
  if oldStyle then Result := LoadOldStyle(rootKey, attrKey, attrName)
              else Result := LoadNewStyle(rootKey, attrKey, attrName);
end; { TmwHighLightAttributes.LoadFromBorlandRegistry }

procedure TmwHighLightAttributes.SetBackground(Value: TColor);
begin
  if fBackGround <> Value then
    begin
      fBackGround := Value;
      if Assigned(fOnChange) then
        fOnChange(Self);
    end;
end;

procedure TmwHighLightAttributes.SetForeground(Value: TColor);
begin 
  if fForeGround <> Value then
    begin
      fForeGround := Value;
      if Assigned(fOnChange) then
        fOnChange(Self);
    end;
end;

procedure TmwHighLightAttributes.SetStyle(Value: TFontStyles);
begin 
  if fStyle <> Value then
    begin
      fStyle := Value;
      if Assigned(fOnChange) then
        fOnChange(Self);
    end;
end;

function TmwHighLightAttributes.LoadFromRegistry(Reg: TBetterRegistry): boolean;
var
  key: string;
begin
  key := Reg.CurrentPath;
  if Reg.OpenKeyReadOnly(Name) then begin
    if Reg.ValueExists('Background') then Background := Reg.ReadInteger('Background');
    if Reg.ValueExists('Foreground') then Foreground := Reg.ReadInteger('Foreground');
    if Reg.ValueExists('Style') then IntegerStyle := Reg.ReadInteger('Style');
    reg.OpenKeyReadOnly('\'+key);
    Result := true;
  end
  else Result := false;
end;

function TmwHighLightAttributes.SaveToRegistry(Reg: TBetterRegistry): boolean;
var
  key: string;
begin
  key := Reg.CurrentPath;
  if Reg.OpenKey(Name,true) then begin
    Reg.WriteInteger('Background', Background);
    Reg.WriteInteger('Foreground', Foreground);
    Reg.WriteInteger('Style', IntegerStyle);
    reg.OpenKey('\'+key,false);
    Result := true;
  end
  else Result := false;
end;

function TmwHighLightAttributes.GetStyleFromInt: integer;
begin
  if fsBold in Style then Result:= 1 else Result:= 0;
  if fsItalic in Style then Result:= Result+2;
  if fsUnderline in Style then Result:= Result+4;
  if fsStrikeout in Style then Result:= Result+8;
end;

procedure TmwHighLightAttributes.SetStyleFromInt(const Value: integer);
begin
  if Value and $1 = 0 then  Style:= [] else Style:= [fsBold];
  if Value and $2 <> 0 then Style:= Style+[fsItalic];
  if Value and $4 <> 0 then Style:= Style+[fsUnderline];
  if Value and $8 <> 0 then Style:= Style+[fsStrikeout];
end;

{ TmwCustomHighLighter }

constructor TmwCustomHighLighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fAttributes := TStringList.Create;
  fAttributes.Duplicates := dupIgnore;
  fAttributes.Sorted := TRUE;

  fAttrChangeHooks := TmwNotifyEventChain.CreateEx(Self);
  fDefaultFilter := '';
end;

destructor TmwCustomHighLighter.Destroy;
var i: integer;
begin

  for i := fAttributes.Count - 1 downto 0 do begin
    if (fAttributes.Objects[i] = nil) then continue;
    TmwHighLightAttributes(fAttributes.Objects[i]).Free;
  end;
  fAttributes.Free;

  fAttrChangeHooks.Free;
  inherited Destroy;
end;

procedure TmwCustomHighLighter.EnumUserSettings(settings: TStrings);
begin
  settings.Clear;
end;

function TmwCustomHighLighter.UseUserSettings(
  settingIndex: integer): boolean;
begin
  Result := false;
end;

function TmwCustomHighLighter.GetIdentChars: TIdentChars;
begin
  Result := [#33..#255];
end;

procedure TmwCustomHighLighter.NextToEol;
begin
  while not GetEol do Next;
end;

procedure TmwCustomHighLighter.ScanAllLineTokens(const Value: string;
                                                 LineNumber: integer);
var sToken: string;
begin
  SetLine(Value, LineNumber);
  while not GetEOL do begin
    if Assigned(fOnToken) then begin
      sToken := GetToken;
      if (Length(sToken) > 0) then
        OnToken(Self, GetTokenKind, sToken, LineNumber);
    end;
    Next;
  end;
end;

function TmwCustomHighLighter.LoadFromRegistry(RootKey: HKEY; Key: string): boolean;
var
  r: TBetterRegistry;
  i: integer;
begin
  r := TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKeyReadOnly(Key) then begin
      Result := true;
      for i := 0 to AttrCount-1 do
        Result := Result and Attribute[i].LoadFromRegistry(r);
    end
    else Result := false;
  finally r.Free; end;
end;

function TmwCustomHighLighter.SaveToRegistry(RootKey: HKEY; Key: string): boolean;
var
  r: TBetterRegistry;
  i: integer;
begin
  r := TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKey(Key,true) then begin
      Result := true;
      for i := 0 to AttrCount-1 do
        Result := Result and Attribute[i].SaveToRegistry(r);
    end
    else Result := false;
  finally r.Free; end;
end;

procedure TmwCustomHighLighter.AddAttribute(AAttrib: TmwHighLightAttributes);
begin
  fAttributes.AddObject(AAttrib.Name, AAttrib);
end;

procedure TmwCustomHighLighter.DefHighlightChange(Sender: TObject);
begin
  fAttrChangeHooks.Fire;
end;

function TmwCustomHighLighter.GetAttribCount: integer;
begin
  Result := fAttributes.Count;
end;

function TmwCustomHighLighter.GetAttribute(idx: integer): TmwHighLightAttributes;
begin
  Result := nil;
  if (idx >= 0) and (idx < fAttributes.Count) then
    Result := TmwHighLightAttributes(fAttributes.Objects[idx]);
end;

procedure TmwCustomHighLighter.SetAttributesOnChange(AEvent: TNotifyEvent);
var i: integer;
    attri: TmwHighLightAttributes;
begin
  for i := fAttributes.Count - 1 downto 0 do begin
    attri := TmwHighLightAttributes(fAttributes.Objects[i]);
    if Assigned(attri) then attri.OnChange := AEvent;
  end;
end;

function TmwCustomHighLighter.GetCapability: THighlighterCapability;
begin
  Result := [hcRegistry]; //registry save/load supported by default
end;

function TmwCustomHighLighter.GetDefaultFilter: string;
begin
  Result := fDefaultFilter;
end;

procedure TmwCustomHighLighter.SetDefaultFilter(Value: string);
begin
  if fDefaultFilter <> Value then fDefaultFilter := Value;
end;

procedure TmwCustomHighLighter.HookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
begin
  fAttrChangeHooks.Add(ANotifyEvent);
end;

procedure TmwCustomHighLighter.UnhookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
begin
  fAttrChangeHooks.Remove(ANotifyEvent);
end;

end.
