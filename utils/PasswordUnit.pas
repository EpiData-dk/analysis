unit PasswordUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TPasswordForm = class(TForm)
    edPW1: TEdit;
    Label1: TLabel;
    edPW2: TEdit;
    Label2: TLabel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Label3: TLabel;
    Label4: TLabel;
    lbDatafile: TLabel;
    lblNotIdentical: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    DoubleEntry: Boolean;
  end;

var
  PasswordForm: TPasswordForm;

implementation

{$R *.DFM}

procedure TPasswordForm.FormCreate(Sender: TObject);
begin
  DoubleEntry:=True;
end;

procedure TPasswordForm.FormPaint(Sender: TObject);
begin
  IF DoubleEntry=False THEN
    BEGIN
      edPW2.Visible:=False;
      edPW2.Enabled:=False;
      Label2.Visible:=False;
    END;
end;

procedure TPasswordForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  lblNotIdentical.Visible:=false;
  IF ModalResult=mrCancel THEN CanClose:=True
  ELSE
    BEGIN
      IF DoubleEntry THEN
        BEGIN
          CanClose:=(edPW1.Text=edPW2.Text);
          IF (NOT CanClose) THEN
            begin
              lblNotIdentical.Caption:='The passwords are not identical. Please try again.';
              lblNotIdentical.Visible:=true;
            end;
          IF ((Length(edPW1.Text)<6) OR (trim(edPW1.Text)='')) and (CanClose) THEN
            BEGIN
              lblNotIdentical.Caption:='Password must be 6 characters or more.';
              lblNotIdentical.Visible:=true;
              CanClose:=False;
            END;
        END
      ELSE CanClose:=True;
    END;
end;

end.
