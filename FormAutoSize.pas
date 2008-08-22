{***************************************************************

   Unit Name: MPSAutoSize

   Author   : Robert Wachtel
 						 RWachtel@gmx.de
    					 http://www.mpscologne.de

   History  : V 0.2 02.01.1999
   						2nd Release Version
               Added FontResize Property
               Added TryComponentResize Property
               No additional testing
   					 V 0.1 01.31.1999
              	1st Release Version
               Only marginal testing

		Written and tested in Delphi 3.02 Prof.

   Should work with every Delphi version 3.x and above.

   MPSAutoSize is a non-visual-component written to simplify the writing
   	of Delphi applications for different screen resolutions and fonts. Just
     put the component on the form and activate the settings you will need.

   Installing:
   ===========

			In the menu bar open COMPONENTS and select INSTALL COMPONENTS.
     Then install this unit with the OK button.

   Properties:
   ===========

   	Active
     	set to TRUE if you want the component to be active (what else ;-)

     AllowAutoSize
     	set to TRUE if you want the component to take care of the size of the
       form and it's components in different screen resolutions
       AllowAutoSize only works if Form.Scaled property is true

     ClientAutoSize
     	set to TRUE if you want the component to take care of the Forms's client
       size (useful if you just want the client size to be adjusted due to
       different caption bar font sizes)
       ClientAutoSize is independent of Form.Scaled property, but I recommend
       to use it only with Form.Scaled set to FALSE

     CheckPixelsPerInch
     	if set to TRUE the component will check for different screen font size
       and will adjust the height and width of the parent form if AllowAutoSize
       is set to TRUE

     CheckFontSize
     	if set to TRUE the component will check for the proportion of the sizes
       of the form's fonts (in conjunction with AllowAutoSize)

     DesignTimeSize
     	Width, Height, ClientHeight, PixelsPerInch (what should I say ? ;-)

     Name
     	the name

     Tag
     	the tag

   Tip:
   ====

   	Don't put this component on the form before you haven't finished
     designing the form - otherwise you'll have to change the component's
     DesignTimeSize properties manually (at least the ClientHeight property).

   Miscellaneous:
		==============

   This Component is FreeWare.

   If you have any problems or suggestions please feel free
   	to contact me.

   If you improve or change the code, please be so kind to send me a copy.

   Credits:
   ========

   Thanks to
   	Bea for all the love
			Simon Reinhardt for his DelphiFAQ (http://sr-soft.wtal.de/delphi.htm)
   	Lloyd Linklater for his Delphi Notes Help File
     Sven Eisenkraemer for a lot of useful hints in programming Delphi
     everyone in news://de.comp.lang.pascal.delphi for being a great community

****************************************************************}

unit FormAutoSize;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, TypInfo;

type
	 TFooClass = class(TControl);	// needed to get at protected font property
  															// (see Borprise TI2861)

type
	 TDesignTimeSize = class(TPersistent)
  private
    { Private-Deklarationen }
    FWidth: integer;
    FHeight: integer;
    FClientHeight: integer;
    FPixelsPerInch: integer;
  protected
    { Protected-Deklarationen }
  public
    { Public-Deklarationen }
  published
    { Published-Deklarationen }
    property Width: integer read FWidth write FWidth;
    property Height: integer read FHeight write FHeight;
    property ClientHeight: integer read FClientHeight write FClientHeight;
    property PixelsPerInch: integer read FPixelsPerInch write FPixelsPerInch;
  end;

type
  TMPSAutoSize = class(TComponent)
  private
    { Private-Deklarationen }
    FDesignTimeSize: TDesignTimeSize;
    FClientAutoSize: boolean;
    FAllowAutoSize: boolean;
    FActive: boolean;
    FPPIAware: boolean;
    FFormShow: TNotifyEvent;
    FCheckFontSize: boolean;
    procedure SetClientAutoSize(flag: boolean);
    procedure SetAllowAutoSize(flag: boolean);
    procedure FormShow(Sender: TObject);
    procedure DoClientSizing;
    procedure DoAutoSizing;
  protected
    { Protected-Deklarationen }
    procedure Loaded; override;
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published-Deklarationen }
    property Active: boolean read FActive write FActive;
    property DesignTimeSize: TDesignTimeSize read FDesignTimeSize write FDesignTimeSize;
    property ClientAutoSize: boolean read FClientAutoSize write SetClientAutoSize default false;
    property AllowAutoSize: boolean read FAllowAutoSize write SetAllowAutoSize default false;
    property CheckPixelsPerInch: boolean read FPPIAware write FPPIAware default false;
    property CheckFontSize: boolean read FCheckFontSize write FCheckFontSize default false;
  end;

procedure Register;

const
	FontType = 'font';

implementation

//******************* RegisterComponent *************************
procedure Register;
begin
  RegisterComponents('EpiData', [TMPSAutoSize]);
end;

//******************* TMPSAutoSize.Create *************************
constructor TMPSAutoSize.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDesignTimeSize := TDesignTimeSize.Create;
  if (AOwner is TCustomForm) and (csDesigning in ComponentState) then	// if component is in
  	with FDesignTimeSize do begin																				// design state the
   	FClientHeight := TForm(AOwner).ClientHeight;											// actual screen
  		FWidth := Screen.Width;                                           // dimensions are
  		FHeight := Screen.Height;                                         // stored in
     FPixelsPerInch := TForm(AOwner).PixelsPerInch;                    // properties
   end;
end;

//******************* TMPSAutoSize.Destroy *************************
destructor TMPSAutoSize.Destroy;
begin
  FDesignTimeSize.Free;
  FDesignTimeSize := nil;
  inherited Destroy;
end;

//******************* TMPSAutoSize.Loaded *************************
procedure TMPSAutoSize.Loaded;
var
  Loading: boolean;
begin
  Loading := csLoading in ComponentState;
  inherited Loaded;
  if (FActive)
  	and (not (csDesigning in ComponentState))
   and (Loading)
   and (Owner is TCustomForm) then
  		with TForm(Owner) do begin																				// override the
   		FFormShow := OnShow;																						// owner form's
     	OnShow := FormShow;                                             // OnShow event
  		end;
end;

//******************* SetClientAutoSize *************************
// if setting the ClientAutoSize property to TRUE
// set the AllowAutoSize property to FALSE
procedure TMPSAutoSize.SetClientAutoSize(flag: boolean);
begin
	if (flag <> FClientAutoSize) then begin
 	if (flag) then FAllowAutoSize := false;
   FClientAutoSize := flag;
 end;
end;

//******************* SetAllowAutoSize *************************
// if setting the AllowAutoSize property to TRUE
// set the ClientAutoSize property to FALSE
procedure TMPSAutoSize.SetAllowAutoSize(flag: boolean);
begin
	if (flag <> FAllowAutoSize) then begin
 	if (flag) then FClientAutoSize := false;
   FAllowAutoSize := flag;
 end;
end;

//******************* TMPSAutoSize.FormShow *************************
// called right before the original owner form's OnShow event
procedure TMPSAutoSize.FormShow(Sender: TObject);
begin
	if (FActive) then
 	try
     if (FClientAutoSize) then DoClientSizing;
     if (FAllowAutoSize) then DoAutoSizing;
     // calculate the form's dimensions only
     // at first OnShow event - to be honest
     // I don't know if this is correct ;-)
     FActive := false;
     // so maybe just delete it .....
   except
   	Application.HandleException(self);
   end;
  if Assigned(FFormShow) then FFormShow(Sender);												// "inherited"
end;

//******************* TMPSAutoSize.DoClientSizing *************************
procedure TMPSAutoSize.DoClientSizing;
begin
	with TForm(Owner) do
  	if (ClientHeight <> FDesignTimeSize.FClientHeight) then
  		ClientHeight := FDesignTimeSize.FClientHeight;
end;

//******************* TMPSAutoSize.DoAutoSizing *************************
procedure TMPSAutoSize.DoAutoSizing;
var
	i, OldFormWidth, NewFormWidth: integer;
begin
	with TForm(Owner) do begin
 	OldFormWidth := Width;
		if ((Screen.Width <> FDesignTimeSize.FWidth)
   	or (Screen.Height <> FDesignTimeSize.FHeight))
   	and (Scaled) then begin
     	// calculate the form's dimensions after change of resolution
       Height := (ClientHeight * Screen.Height DIV FDesignTimeSize.FHeight) + Height - ClientHeight;
       Width := (ClientWidth * Screen.Width DIV FDesignTimeSize.FWidth) + Width - ClientWidth;
       ScaleBy(Screen.Width, FDesignTimeSize.FWidth);
     end;
   if (FPPIAware) and (PixelsPerInch <> FDesignTimeSize.FPixelsPerInch) then begin
   	// calculate the form's dimensions depending on the PixelsPerInch settings
   	Height := Height * PixelsPerInch DIV FDesignTimeSize.FPixelsPerInch;
     Width := Width * PixelsPerInch DIV FDesignTimeSize.FPixelsPerInch;
   end;
   NewFormWidth := Width;
		// enumerate all form's controls and check font sizes
   if (FCheckFontSize) and (NewFormWidth <> OldFormWidth) then
	  	for i := ControlCount - 1 downto 0 do
     	with TFooClass(Controls[i]) do
       	if not ParentFont then Font.Size := Font.Size * NewFormWidth DIV OldFormWidth;
 end;
end;

end.
