unit Tbuttonled;
{ TButtonLed: Marco Caselli's Led Button rel. 1.0
 A button with a led inside to indicate the on/off state of something
 You can customize color of the button and led.
**********************************************************************
* Feel free to use or give away this software as you see fit.        *
* Please leave the credits in place if you alter the source.         *
*                                                                    *
* This software is delivered to you "as is",                         *
* no guarantees of any kind.                                         *
*                                                                    *
* If you find any bugs, please let me know, I will try to fix them.  *
* If you modify the source code, please send me a copy               *
* Marco Caselli                                                      *
* Web site : http://members.tripod.com/dartclub                      *
* E-mail   : mcaselli@iname.com                                      *
**********************************************************************
*** Sorry for my bad english ...............
 Key Properties :

       Color      : Color of the surface of button;
       FalseColor : Color of led in OFF state;
       TrueColor  : Color of led in ON state;
       State      : On/Off state;
       LedRadius  : The radius in pixel of the led;
       Position   : Where to put led in button surface;
       FromBorder : Distance from Button Border;

*******************************************************************************}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,buttons;

type
  TLocation=(poTopLeft,poTopRight,poBottonRight,poBottomLeft);
  TmButton = class(tCustomControl)
  private
    fTrueColor       : TColor;
    fFalseColor      : TColor;
    fState           : Boolean;
    fButtonState     : TButtonState;
    fDown            : Boolean;
    fPosition        : TLocation;
    fLedRadius       : Word;
    fFromBorder      : Word;
    procedure SetTrueColor(Value:TColor);
    procedure SetFalseColor(Value:TColor);
    procedure SetFromBorder(Value:Word);
    procedure SetState(Value:Boolean);
    procedure SetLedRadius(Value:Word);
    procedure SetPosition(Value:TLocation);
    { Private declarations }
  protected
     constructor Create(AOwner:TComponent); override;
     procedure Paint; override;
     procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
     procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
     procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
     procedure Redraw(AOwner:TObject);
    { Protected declarations }
  public
    procedure Click; override;
    procedure DblClick; override;
    { Public declarations }
  published
    Property Position:TLocation read fPosition write SetPosition;
    Property State:Boolean read fstate write SetState default false;
    property TrueColor:TColor read fTrueColor write SetTrueColor;
    property FalseColor:TColor read fFalseColor write SetFalseColor;
    property FromBorder:Word read fFromBorder write SetFromBorder;
    Property LedRadius:Word read fLedRadius write SetLedRadius;
    {Publishing parent properties..}
    property Color;
    Property Enabled;
    property Font;
    property onClick;
    property onDblClick;
    property onMouseMove;
    property onMouseDown;
    Property onMouseUp;
    property onEnter;
    property onExit;
    property Caption;

    { Published declarations }
  end;

procedure Register;

implementation

constructor TmButton.Create(AOwner:TComponent);
begin
   inherited create(AOwner);
   State:=False;
   TrueColor:=clRed;
   FalseColor:=clBlack;
   Caption:=Name;
   LedRadius:=10;
   FromBorder:=5;
   Position:=poTopLeft;
   Font.onChange:=Redraw;
   DragMode:=dmManual;
 end;


procedure TmButton.SetTrueColor(Value:TColor);
begin
   if fTrueColor <> Value then
      begin
         fTrueColor:=Value;
         repaint;
      end;
end;

procedure TmButton.SetFromBorder(Value:Word);
begin
   if fFromBorder <> Value then
      begin
         fFromBorder:=Value;
         repaint;
      end;
end;

procedure TmButton.SetPosition(Value:TLocation);
begin
   if fPosition <> Value then
      begin
         fPosition:=Value;
         repaint;
      end;
end;

procedure TmButton.SetLedRadius(Value:Word);
begin
   if fLedRadius <> Value then
      begin
         fLedRadius:=Value;
         repaint;
      end;
end;


procedure TmButton.SetFalseColor(Value:TColor);
begin

   if fFalseColor <> Value then
      begin
         fFalseColor:=Value;
         Repaint;
      end;
end;
procedure Tmbutton.Redraw(AOwner:Tobject);
begin
  Paint;
end;

procedure TmButton.SetState(Value:Boolean);
begin
   if fState <> Value then
      begin
         fState:=Value;
         Repaint;
      end;
end;

procedure TmButton.Click;
begin
//  State:= not State;
  inherited Click;
end;

procedure TmButton.DblClick;
begin
//  State:= not State;
  inherited DblClick;
end;


procedure tmbutton.paint;
var
  Area:TRect;
  Led:TRect;
  FromBorder:Integer;
  Distance:Integer;
begin
     inherited paint;
     if not Enabled and not (csDesigning in ComponentState) then
        if fButtonState = bsDisabled then fButtonState := bsUp;
     Area:=DrawButtonFace(Canvas,Rect(0,0,Width,Height),2,bsNew,False,fButtonState  in [bsDown],false);
     canvas.brush.color:=Color;
     canvas.fillrect(Area);
     with canvas do
        begin
         if caption <> '' then
            begin
              font.assign(self.Font);
              drawtext(handle,pchar(caption),length(caption),Area,DT_Center or dt_Vcenter or Dt_SIngleline);
            end;

         if fState then
             Brush.Color:=fTrueColor
         else
             Brush.Color:=fFalseColor;

         Distance:=fLedRadius + fFromBorder;
    { I think that this can be done in a better way, but .... IT WORK!}
         case fPosition of
              poTopLeft    : led:=Rect(Area.left+fFromBorder,Area.top+fFromBorder,
                                   Area.left+Distance,Area.top+Distance);

              poTopRight   : led:=Rect(Area.right-Distance,Area.top+fFromBorder,
                                   Area.right-fFromBorder,Area.top+Distance);

              poBottonRight: led:=Rect(Area.right-Distance,Area.Bottom-Distance,
                                   Area.right-fFromBorder,Area.Bottom-fFromBorder);

              poBottomLeft : led:=Rect(Area.Left+fFromBorder,Area.Bottom-Distance,
                                   Area.Left+Distance,Area.Bottom-fFromBorder);
         end;

         Ellipse(led.Left,led.Top,led.Right,led.Bottom);
       end;
end;

procedure TmButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
{  if (Button = mbLeft) and Enabled then
  begin
    fButtonState := bsDown;
    Fdown:=true;
    Repaint;
  end;}
 inherited MouseDown(Button, Shift, X, Y);
end;

procedure TmButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TButtonState;
begin
{
  if (shift =[ssLeft]) then begin
    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      NewState := bsdown else NewState := bsUp;
    if NewState <> FbuttonState then
    begin
      FbuttonState := NewState;
      Repaint;
    end;
  end;}
  inherited MouseMove(Shift, X, Y);
end;


procedure TmButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  DoClick: Boolean;
begin
{  begin
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    FbuttonState := bsUp;
  end;
  repaint;}
  inherited MouseUp(Button, Shift, X, Y);
end;


procedure Register;
begin
  RegisterComponents('EpiData', [TmButton]);
end;

end.
