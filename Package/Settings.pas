{
	Original Filename: Settings. Version 1.0
 Author											: Paul van Dinther
 Company										: Diprode
 Website										: http://www.diprode.com
 e-mail											:	paul@diprode.com
 Copyright								:	Diprode, 2001
}

unit Settings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, registry, inifiles, dbTables,Variants;

type
  TSettingCollection     = class;
  TSettingItem     = class;
  TSettingStorage = (ssRegistry, ssIniFile, ssDatabase);
  TSettingType = (stBoolean, stCurrency, stDate, stDateTime, stFloat, stInteger, stString, stTime);
  TRootKeys = (rkHKEY_CLASSES_ROOT, rkHKEY_CURRENT_USER, rkHKEY_LOCAL_MACHINE, rkHKEY_USERS, rkHKEY_PERFORMANCE_DATA, rkHKEY_CURRENT_CONFIG, rkHKEY_DYN_DATA);

  TSettings = class(TComponent)
  private
    FRegistry					: TRegistry;
    FIniFile						: TIniFile;
    FTable								: TTable;
    FAutoSave					: Boolean;
    FSettings     : TSettingCollection;
    FDatabaseName	: String;
    FTableName				: String;
    FActive							:	Boolean;
    FIniFileName		: TFileName;
    FRootKey      : TRootKeys;
    FBaseKey						: String;
    FFilter							: String;
    FOnChange					: TNotifyEvent;
    procedure SetAutoSave(PValue: Boolean);
    procedure SetSettings(PValue : TSettingCollection);
    procedure SetDatabasename(PValue: String);
   procedure SetTableName(PValue: String);
   procedure CreateDBAccess;
    procedure SetActive(PValue: Boolean);
    procedure SetIniFileName(PValue: TFileName);
   procedure SetRootKey(PValue: TRootKeys);
    procedure SetBaseKey(PValue: String);
    procedure SetFilter(PValue: String);
    procedure WriteSettings(Writer: TWriter);
    procedure ReadSettings(Reader: TReader);
   function CombineKey(PKey1, PKey2: String): String;
   procedure SaveItemToRegistry(PSettingItem: TSettingItem);
   	procedure SaveItemToIniFile(PSettingItem: TSettingItem);
   	procedure SaveItemToDataBase(PSettingItem: TSettingItem);
   	procedure LoadItemFromRegistry(PSettingItem: TSettingItem);
   	procedure LoadItemFromIniFile(PSettingItem: TSettingItem);
   	procedure LoadItemFromDataBase(PSettingItem: TSettingItem);
  protected
    procedure DefineProperties(Filer : TFiler); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function Load: Boolean;
    function Save: Boolean;
    function ByName(PName: String): TSettingItem;
    procedure UpdateSetting;
  published
   property AutoSave  	: Boolean read FAutoSave write SetAutoSave;
   property Active    	: Boolean read FACtive write SetActive;
   property Filter    	: String read FFilter write SetFilter;
   property IniFileName	 : TFileName read FIniFileName write SetIniFileName;
   property Settings     : TSettingCollection read FSettings     write SetSettings;
   property DatabaseName : string read FDatabasename write SetDatabasename;
   property TableName 	: String read FTableName write SetTableName;
   property RootKey   	: TRootKeys read FRootKey write SetRootKey;
   property BaseKey   	: string read FBaseKey write SetBaseKey;
   property OnChange  	: TNotifyEvent read FOnChange write FOnChange;
  end;


  TSettingCollection = class(TCollection)
  private
     FSettings : TSettings;
     function  GeTSettingItem(index : integer)   : TSettingItem;
     procedure SetSettingItem(index : integer; PValue : TSettingItem);
  protected
     function GetOwner : TPersistent; override;
     procedure Update(Item: TCollectionItem); override;
  public
     function Add : TSettingItem;
     property Items[index : integer] : TSettingItem read GeTSettingItem write SeTSettingItem;
     constructor create(AOwner : TSettings);
  end;

  TSettingItem = class(TCollectionItem)
  private
     FName : string;
     FDataType: TSettingType;
     FStorage: TSettingStorage;
     FValue: Variant;
     FKey: String;
     FFieldName: String;
     procedure SetName(PValue : string);
     procedure SetDataType(PValue: TSettingType);
     procedure SetStorage(PValue: TSettingStorage);
     procedure SetValue(PValue: Variant);
     procedure SetKey(PValue: String);
     procedure SetFieldName(PValue: String);
  public
     constructor Create(Collection : TCollection); override;
  published
     property Name : string read FName write SetName;
     property DataType : TSettingType read FDataType write SetDataType;
     property FieldName: String read FFieldName write SetFieldName;
     property Key: string read FKey write SetKey;
     property Storage: TSettingStorage read FStorage write SetStorage;
     property Value: Variant read FValue write SetValue;
  end;


procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('EpiData', [TSettings]);
end;

constructor TSettings.Create(AOwner : TComponent);
begin
 inherited;
 FSettings := TSettingCollection.create(self);
 if csDesigning in componentstate then
 begin
  FSettings.Add;
  FSettings.Items[0].Name := Name;
 end;
	FRegistry := TRegistry.Create;
	FRootKey := rkHKEY_CURRENT_USER;
 FBaseKey := 'Software';
 Load;
end;

destructor TSettings.Destroy;
begin
	if assigned(FIniFile) then FIniFile.Free;
	FRegistry.Free;
 inherited;
end;

function TSettings.Load: Boolean;
var
	i: Integer;
begin
	Result := True;
	//Data is assumed to be present on the first record
	if assigned(FTable) then
 begin
 	if FFilter <> '' then
  begin
			FTable.Filter := FFilter;
   FTable.Filtered := True;
  end
  else
  begin
  	FTable.Filter := '';
	  FTable.Filtered := False;
  end;
  FTable.open;
 end;
	for i := 0 to FSettings.Count - 1 do
 begin
		Case FSettings.Items[i].Storage of
			ssRegistry:	LoadItemFromRegistry(FSettings.Items[i]);
   ssIniFile :	LoadItemFromIniFile(FSettings.Items[i]);
   ssDatabase:	LoadItemFromDataBase(FSettings.Items[i]);
  end;
 end;
 if assigned(FTable) then FTable.Close;
end;

function TSettings.Save: Boolean;
var
	i: Integer;
begin
	Result := True;
	if assigned(FTable) then
 begin
 	if FFilter <> '' then
  begin
   FTable.Filter := FFilter;
   FTable.Filtered := True;
  end
  else
  begin
			FTable.Filter := '';
   FTable.Filtered := False;
  end;
  FTable.open;
  FTable.Edit;
 end;
	for i := 0 to FSettings.Count - 1 do
 begin
		Case FSettings.Items[i].Storage of
			ssRegistry:	SaveItemToRegistry(FSettings.Items[i]);
   ssIniFile :	SaveItemToIniFile(FSettings.Items[i]);
   ssDatabase:	SaveItemToDataBase(FSettings.Items[i]);
  end;
 end;
 if assigned(FTable) then
 begin
		FTable.post;
 	FTable.Close;
 end;
end;

function TSettings.ByName(PName: String): TSettingItem;
var
	i: Integer;
begin
 PName := lowercase(PName);
 Result := nil;
 for i := 0 to self.FSettings.Count - 1 do
 begin
  if lowercase(FSettings.Items[i].Name) = PName then
  begin
    Result := FSettings.items[i];
    break;
  end;
 end;
end;

procedure TSettings.SetAutoSave(PValue: Boolean);
begin
 if FAutoSave <> PValue then
 FAutoSave := PValue;
 if FAutoSave then Save;
end;

procedure TSettings.SetSettings(PValue : TSettingCollection);
begin
	if PValue <> FSettings then
 begin
  FSettings := PValue;
 end;
end;

procedure TSettings.SetDatabasename(PValue: String);
begin
	if FdatabaseName <> PValue then
 begin
 	FDatabaseName := PValue;
		CreateDBAccess;
 end;
end;

procedure TSettings.SetTableName(PValue: String);
begin
	if FTableName <> PValue then
 	FTableName := PValue;
		CreateDBAccess;
end;

procedure TSettings.CreateDBAccess;
begin
 if (FDatabaseName <> '') and (FTableName <> '') then
 begin
  if assigned(FTable) then FTable.Free;
  FTable := TTable.Create(self);
  FTable.DatabaseName := FDatabaseName;
  FTable.TableName := TableName;
 end;
end;

procedure TSettings.SetActive(PValue: Boolean);
begin
  FActive := PValue;
  if FActive then Load
  else Save;
end;

procedure TSettings.SetIniFileName(PValue: TFileName);
begin
 if FIniFileName <> PValue then
 begin
  if assigned(FiniFile) then FIniFile.Free;
  FIniFileName := PValue;
  if FiniFileName <> '' then
  begin
   FIniFile := TIniFile.Create(FIniFileName);
   if FActive then Load;
  end;
 end;
end;

procedure TSettings.SetRootKey(PValue: TRootKeys);
begin
	if FRootKey <> PValue then
 	FRootKey := PValue;
end;

procedure TSettings.SetBaseKey(PValue: String);
begin
	if FBaseKey <> PValue then
 	FBaseKey := PValue;
end;

procedure TSettings.SetFilter(PValue: String);
begin
	if FFilter <> PValue then
 	FFilter := PValue;
end;

procedure TSettings.DefineProperties(Filer : TFiler);
begin
 inherited;
	Filer.DefineProperty('Settings', ReadSettings, WriteSettings, Filer.Ancestor <> nil);
end;

procedure TSettings.UpdateSetting;
begin
	if assigned(FOnChange) then FOnChange(self);
	if FAutoSave then Save;
end;

procedure TSettings.WriteSettings(Writer: TWriter);
begin
 Writer.WriteCollection(Settings);
end;

procedure TSettings.ReadSettings(Reader: TReader);
begin
 Settings.Clear;
 Reader.ReadValue;
 Reader.ReadCollection(Settings);
end;

procedure TSettings.SaveItemToRegistry(PSettingItem: TSettingItem);
begin
	case FRootKey of
		rkHKEY_CLASSES_ROOT					:	FRegistry.RootKey := HKEY_CLASSES_ROOT;
	 rkHKEY_CURRENT_USER					: FRegistry.RootKey := HKEY_CURRENT_USER;
	 rkHKEY_LOCAL_MACHINE				: FRegistry.RootKey := HKEY_LOCAL_MACHINE;
	 rkHKEY_USERS												: FRegistry.RootKey := HKEY_USERS;
	 rkHKEY_PERFORMANCE_DATA	: FRegistry.RootKey := HKEY_PERFORMANCE_DATA;
	 rkHKEY_CURRENT_CONFIG			: FRegistry.RootKey := HKEY_CURRENT_CONFIG;
	 rkHKEY_DYN_DATA									: FRegistry.RootKey := HKEY_DYN_DATA;
	end;
 FRegistry.OpenKey(FBaseKey,True);
 FRegistry.OpenKey(PSettingItem.Key,True);
	if assigned(PSettingItem) and (PSettingItem.Name <> '') then
 begin
		Case PSettingItem.DataType of
	  stBoolean	:	FRegistry.WriteBool(PSettingItem.Name, PSettingItem.Value);
	  stCurrency: FRegistry.WriteCurrency(PSettingItem.Name, PSettingItem.Value);
	  stDate				: FRegistry.WriteDate(PSettingItem.Name, PSettingItem.Value);
	  stDateTime: FRegistry.WriteDateTime(PSettingItem.Name, PSettingItem.Value);
	  stFloat			: FRegistry.WriteFloat(PSettingItem.Name, PSettingItem.Value);
	  stInteger	: FRegistry.WriteInteger(PSettingItem.Name, PSettingItem.Value);
	  stString		: FRegistry.WriteString(PSettingItem.Name, PSettingItem.Value);
	  stTime				: FRegistry.WriteTime(PSettingItem.Name, PSettingItem.Value);
	 end;
 end;
 FRegistry.CloseKey
end;

procedure TSettings.SaveItemToIniFile(PSettingItem: TSettingItem);
begin
	if assigned(PSettingItem) and (PSettingItem.Name <> '') then
 begin
		Case PSettingItem.DataType of
	  stBoolean	: FIniFile.WriteBool(CombineKey(FBaseKey,PSettingItem.Key),PSettingItem.Name, PSettingItem.Value);
	  stCurrency: FIniFile.WriteFloat(CombineKey(FBaseKey,PSettingItem.Key),PSettingItem.Name, PSettingItem.Value);
	  stDate				: FIniFile.WriteDate(CombineKey(FBaseKey,PSettingItem.Key),PSettingItem.Name, PSettingItem.Value);
	  stDateTime: FIniFile.WriteDateTime(CombineKey(FBaseKey,PSettingItem.Key),PSettingItem.Name, PSettingItem.Value);
	  stFloat			: FIniFile.WriteFloat(CombineKey(FBaseKey,PSettingItem.Key),PSettingItem.Name, PSettingItem.Value);
	  stInteger	: FIniFile.WriteInteger(CombineKey(FBaseKey,PSettingItem.Key),PSettingItem.Name, PSettingItem.Value);
	  stString		: FIniFile.WriteString(CombineKey(FBaseKey,PSettingItem.Key),PSettingItem.Name, PSettingItem.Value);
	  stTime				: FIniFile.WriteTime(CombineKey(FBaseKey,PSettingItem.Key),PSettingItem.Name, PSettingItem.Value);
	 end;
 end;
end;

function TSettings.CombineKey(PKey1, PKey2: String): String;
begin
if (PKey1 <> '') and (PKey2 <> '') then
	Result := PKey1 + '\' + PKey2
else
	Result := PKey1 + PKey2;
end;

procedure TSettings.SaveItemToDataBase(PSettingItem: TSettingItem);
begin
	if not assigned(FTable) then exit;
	if assigned(PSettingItem) then
 begin
		Case PSettingItem.DataType of
	  stBoolean	: FTable.FieldByName(PSettingItem.FieldName).AsBoolean := PSettingItem.Value;
	  stCurrency: FTable.FieldByName(PSettingItem.FieldName).AsCurrency := PSettingItem.Value;
	  stDate				: FTable.FieldByName(PSettingItem.FieldName).AsDateTime := PSettingItem.Value;
	  stDateTime: FTable.FieldByName(PSettingItem.FieldName).AsDateTime := PSettingItem.Value;
	  stFloat			: FTable.FieldByName(PSettingItem.FieldName).AsFloat := PSettingItem.Value;
	  stInteger	: FTable.FieldByName(PSettingItem.FieldName).AsInteger := PSettingItem.Value;
	  stString		: FTable.FieldByName(PSettingItem.FieldName).AsString := PSettingItem.Value;
	  stTime				: FTable.FieldByName(PSettingItem.FieldName).AsDateTime := PSettingItem.Value;
	 end;
 end;
end;

procedure TSettings.LoadItemFromRegistry(PSettingItem: TSettingItem);
begin
	case FRootKey of
		rkHKEY_CLASSES_ROOT					:	FRegistry.RootKey := HKEY_CLASSES_ROOT;
	 rkHKEY_CURRENT_USER					: FRegistry.RootKey := HKEY_CURRENT_USER;
	 rkHKEY_LOCAL_MACHINE				: FRegistry.RootKey := HKEY_LOCAL_MACHINE;
	 rkHKEY_USERS												: FRegistry.RootKey := HKEY_USERS;
	 rkHKEY_PERFORMANCE_DATA	: FRegistry.RootKey := HKEY_PERFORMANCE_DATA;
	 rkHKEY_CURRENT_CONFIG			: FRegistry.RootKey := HKEY_CURRENT_CONFIG;
	 rkHKEY_DYN_DATA									: FRegistry.RootKey := HKEY_DYN_DATA;
	end;
 FRegistry.OpenKey(FBaseKey,True);
 FRegistry.OpenKey(PSettingItem.Key,True);
	if assigned(PSettingItem) and (PSettingItem.Name <> '') then
 begin
		Case PSettingItem.DataType of
	  stBoolean	:	PSettingItem.Value := FRegistry.ReadBool(PSettingItem.Name);
	  stCurrency: PSettingItem.Value := FRegistry.ReadCurrency(PSettingItem.Name);
	  stDate				: PSettingItem.Value := FRegistry.ReadDate(PSettingItem.Name);
	  stDateTime: PSettingItem.Value := FRegistry.ReadDateTime(PSettingItem.Name);
	  stFloat			: PSettingItem.Value := FRegistry.ReadFloat(PSettingItem.Name);
	  stInteger	: PSettingItem.Value := FRegistry.ReadInteger(PSettingItem.Name);
	  stString		: PSettingItem.Value := FRegistry.ReadString(PSettingItem.Name);
	  stTime				: PSettingItem.Value := FRegistry.ReadTime(PSettingItem.Name);
	 end;
 end;
 FRegistry.CloseKey;
end;

procedure TSettings.LoadItemFromIniFile(PSettingItem: TSettingItem);
begin
	if assigned(PSettingItem) and (PSettingItem.Name <> '') then
 begin
		Case PSettingItem.DataType of
	  stBoolean	: PSettingItem.Value := FIniFile.ReadBool(CombineKey(FBaseKey,PSettingItem.Key),PSettingItem.Name, PSettingItem.Value);
	  stCurrency: PSettingItem.Value := FIniFile.ReadFloat(CombineKey(FBaseKey,PSettingItem.Key),PSettingItem.Name, PSettingItem.Value);
	  stDate				: PSettingItem.Value := FIniFile.ReadDate(CombineKey(FBaseKey,PSettingItem.Key),PSettingItem.Name, PSettingItem.Value);
	  stDateTime: PSettingItem.Value := FIniFile.ReadDateTime(CombineKey(FBaseKey,PSettingItem.Key),PSettingItem.Name, PSettingItem.Value);
	  stFloat			: PSettingItem.Value := FIniFile.ReadFloat(CombineKey(FBaseKey,PSettingItem.Key),PSettingItem.Name, PSettingItem.Value);
	  stInteger	: PSettingItem.Value := FIniFile.ReadInteger(CombineKey(FBaseKey,PSettingItem.Key),PSettingItem.Name, PSettingItem.Value);
	  stString		: PSettingItem.Value := FIniFile.ReadString(CombineKey(FBaseKey,PSettingItem.Key),PSettingItem.Name, PSettingItem.Value);
	  stTime				: PSettingItem.Value := FIniFile.ReadTime(CombineKey(FBaseKey,PSettingItem.Key),PSettingItem.Name, PSettingItem.Value);
	 end;
 end;
end;

procedure TSettings.LoadItemFromDataBase(PSettingItem: TSettingItem);
begin
if not assigned(FTable) then exit;
	if assigned(PSettingItem) then
 begin
		Case PSettingItem.DataType of
	  stBoolean	: PSettingItem.Value := FTable.FieldByName(PSettingItem.FieldName).AsBoolean;
	  stCurrency: PSettingItem.Value := FTable.FieldByName(PSettingItem.FieldName).AsCurrency;
	  stDate				: PSettingItem.Value := FTable.FieldByName(PSettingItem.FieldName).AsDateTime;
	  stDateTime: PSettingItem.Value := FTable.FieldByName(PSettingItem.FieldName).AsDateTime;
	  stFloat			: PSettingItem.Value := FTable.FieldByName(PSettingItem.FieldName).AsFloat;
	  stInteger	: PSettingItem.Value := FTable.FieldByName(PSettingItem.FieldName).AsInteger;
	  stString		: PSettingItem.Value := FTable.FieldByName(PSettingItem.FieldName).AsString;
	  stTime				: PSettingItem.Value := FTable.FieldByName(PSettingItem.FieldName).AsDateTime;
	 end;
 end;
end;

//******************************************************************************

constructor TSettingCollection.create(AOwner : TSettings);
begin
	inherited create(TSettingItem);
 FSettings := AOwner;
end;

function TSettingCollection.GetOwner : TPersistent;
begin
 Result := FSettings;
end;

function  TSettingCollection.GeTSettingItem(index : integer)   : TSettingItem;
begin
 Result := TSettingItem(inherited Items[Index]);
end;

procedure TSettingCollection.SetSettingItem(index : integer; PValue : TSettingItem);
begin
 Items[Index].Assign(PValue);
end;

function TSettingCollection.Add : TSettingItem;
begin
 Result := TSettingItem(inherited Add);
end;

procedure TSettingCollection.Update(Item: TCollectionItem);
begin
 FSettings.UpdateSetting;
end;

//******************************************************************************

constructor TSettingItem.Create(Collection : TCollection);
begin
 inherited;
 if assigned(Collection)
 and (Collection is TSettingCollection) then
	 inherited Create(Collection);
end;

procedure TSettingItem.SetName(PValue : string);
begin
	if PValue <> FName then
 begin
  FName := PValue;
		DisplayName := PValue;
  Changed(false);
 end;
end;

procedure TSettingItem.SetDataType(PValue: TSettingType);
begin
 if PValue <> FDataType then
 begin
  FDataType := PValue;
  Changed(false);
 end;
end;

procedure TSettingItem.SetStorage(PValue: TSettingStorage);
begin
 if PValue <> FStorage then
 begin
  FStorage := PValue;
  Changed(false);
 end;
end;

procedure TSettingItem.SetValue(PValue: Variant);
begin
	if VarIsEmpty(FValue) or VarIsEmpty(PValue) or (FValue <> PVAlue) then
	begin
 	FValue := PValue;
	 Changed(false);
 end;
end;

procedure TSettingItem.SetKey(PValue: String);
begin
 if PValue <> FKey then
 begin
  FKey := PValue;
  Changed(false);
 end;
end;

procedure TSettingItem.SetFieldName(PValue: String);
begin
 if PValue <> FFieldName then
 begin
  FFieldName := PValue;
  Changed(false);
 end;
end;



end.
