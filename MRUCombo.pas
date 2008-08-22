{
Classes     : TMRUComboBox component
Version     :  0.01
Author      : Theo Bebekis, bebekis@otenet.gr
Description : a most recently used string combo box
Use         : Just set some properties so the control
              to know where to save its string list
              and then call ReadList or WriteList
              to read or store the list
}

{-------------------------------------------------------
 STATE: No restrictions,
        No guaranties
        Use it at your own risk!
--------------------------------------------------------}

{HISTORY
-----------------------------------------------------------------------------------------
 Version   Date        Autor      Changes - Additions
-----------------------------------------------------------------------------------------
 0.01      Unknown                Initial release
 0.02      2000-01-27  riceball   remove store/read Reg, so you must set IniSection
                                  and INIFile. when you set INIFile, it will Read
                                  History List in.
                                  modified and added methods
                                  if want save history list, pls added follow code
                                  in FormClose:
                                     For i:= 0 to CompoenentCount - 1 do
                                        if Components[i] is TMRUComboBox then
                                          Components[i] as TMRUComboBox.WriteList;}

Unit MRUCombo;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  TypInfo,
  StdCtrls,
  Registry,
  IniFiles;
Type
  {$IFDEF VER130}
  TInteger = Int64;
  {$ELSE}
  TInteger = Integer;
  {$ENDIF}

  TRootKey = (hkClassesRoot, hkCurrentConfig, hkCurrentUser, hkDynData, hkLocalMachine, hkUsers);
  TSaveListTo = (slRegistry, slIniFile);

  //HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH TMRUComboBox HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH

  TMRUComboBox = Class(TComboBox)
  Private
    FIniFile: String;
    FRootKey: TRootKey;
    FSaveList: TSaveListTo;
    FIniSection: String;
    FMaxListLength: Integer;
    Procedure SetIniSection(Value: String);
    Procedure SetRootKey(Value: TRootKey);
    procedure SetSaveList(Value: TSaveListTo);
    Procedure ReadIni;
    Procedure WriteIni;
    Procedure ReadReg;
    Procedure WriteReg;
    Procedure SetIniFile(Value: String);
  Protected
    Procedure Loaded; Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Procedure AddItemToList(NewItem: String);
    Procedure ReadList;
    Procedure WriteList;
  Published
    Property RegRootKey: TRootKey Read FRootKey Write SetRootKey;
    Property SaveListMode: TSaveListTo Read FSaveList Write SetSaveList;
    Property IniFile: String Read FIniFile Write SetIniFile;
    Property IniSection: String Read FIniSection Write SetIniSection;
    Property MaxListLength: Integer Read FMaxListLength Write FMaxListLength Default 10;
  End;

Procedure Register;

Function StripAllSpaces(S: String): String;

Implementation

Constructor TMRUComboBox.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  MaxListLength := 10;
End; { Create }

Procedure TMRUComboBox.Loaded;
Begin
  Inherited;
  ReadList;
End;

Procedure TMRUComboBox.ReadList;
Begin
  if FSaveList = slRegistry then
    ReadReg;
  if FSaveList = slIniFile then
    ReadIni;
End; { ReadList  }

Procedure TMRUComboBox.WriteList;
Begin
  if FSaveList = slRegistry then
    WriteReg;
  if FSaveList = slIniFile then
    WriteIni;
End; {  WriteList }

Procedure TMRUComboBox.SetIniFile(Value: String);
Begin
  If Value <> FIniFile Then Begin
    FIniFile := Value;
  End;
End;

Procedure TMRUComboBox.SetRootKey(Value: TRootKey);
Begin
  If Value <> FRootKey Then Begin
    FRootKey := Value;
  End;
End;

Procedure TMRUComboBox.SetSaveList(Value: TSaveListTo);
Begin
  If Value <> FSaveList Then Begin
    FSaveList := Value;
  End;
End;

Procedure TMRUComboBox.ReadReg;
Var
  List: TStringList;
  i: Integer;
Begin
  If (StripAllSpaces(FIniFile) = '') Or (StripAllSpaces(FIniSection) = '') Then Exit;

  List := TStringList.Create;
  With TRegIniFile.Create(FIniFile) Do Begin
    Try
      Clear;
      ReadSectionValues(FIniSection, List);
      For i := 0 To List.Count - 1 Do
        Items.Add(List.Values['MRU_' + IntToStr(i)]);
    Finally
      List.Free;
      Free;
    End;
  End;
End; { ReadReg  }

Procedure TMRUComboBox.WriteReg;
Var
  i: Integer;
Begin
  If (StripAllSpaces(FIniFile) = '') Or (StripAllSpaces(FIniSection) = '') Then Exit;

  With TRegIniFile.Create(FIniFile) Do Begin
    Try
      EraseSection(FIniSection); // Erase the Section first
      AddItemToList(Text); // Add Self.Text to Items
      For i := 0 To Items.Count - 1 Do // Write Items to Ini
        WriteString(FIniSection, 'MRU_' + IntToStr(i), Items[i]);
    Finally
      Free;
    End;
  End;
End; { WriteReg  }

Procedure TMRUComboBox.ReadIni;
Var
  List: TStringList;
  i: Integer;
Begin
  If (StripAllSpaces(FIniFile) = '') Or (StripAllSpaces(FIniSection) = '') Then Exit;

  List := TStringList.Create;
  With TIniFile.Create(FIniFile) Do Begin
    Try
      Clear;
      ReadSectionValues(FIniSection, List);
      For i := 0 To List.Count - 1 Do
        Items.Add(List.Values['MRU_' + IntToStr(i)]);
    Finally
      List.Free;
      Free;
    End;
  End;
End; { ReadIni  }

Procedure TMRUComboBox.WriteIni;
Var
  i: Integer;
Begin
  If (StripAllSpaces(FIniFile) = '') Or (StripAllSpaces(FIniSection) = '') Then Exit;

  With TIniFile.Create(FIniFile) Do Begin
    Try
      EraseSection(FIniSection); // Erase the Section first
      AddItemToList(Text); // Add Self.Text to Items
      For i := 0 To Items.Count - 1 Do // Write Items to Ini
        WriteString(FIniSection, 'MRU_' + IntToStr(i), Items[i]);
    Finally
      Free;
    End;
  End;
End; { WriteIni  }

Procedure TMRUComboBox.AddItemToList(NewItem: String);
Var
  i, ItemsCounter: Integer;
Begin {add a string to dropdown list}
  If (NewItem <> '') Then Begin
    Items.Insert(0, NewItem); // Insert the New Item in first position
    i := 1; // Check maximum number of entries and delete any duplicate
    ItemsCounter := Items.Count;
    While i < ItemsCounter Do Begin
      If (Items[i] = NewItem) // if there is any duplicate
      Or (i > FMaxListLength - 1) Then Begin
        // or the maximum number of entries is reached
        Items.Delete(i);
        Dec(ItemsCounter);
      End
      Else Inc(i);
    End;
    Text := Items[0];
  End;
End; { AddItemToList }

Procedure TMRUComboBox.SetIniSection(Value: String);
Begin
  If UpperCase(Value) <> UpperCase(FIniSection) Then FIniSection := Value;
End; { SetIniSection  }

Function StripAllSpaces(S: String): String;
Var
  Size: Integer;
  i: Integer;
Begin
  Size := Length(S);
  For i := 1 To Size Do
    If S[i] = ' ' Then Delete(S, i, 1);

  Result := S;

End; { StripAllSpaces }

Procedure Register;
Begin
  RegisterComponents('EpiData', [TMRUComboBox]);
End;

End.

