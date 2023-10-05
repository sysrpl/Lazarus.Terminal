unit NoVte;

{$i terminal.inc}

interface

{$ifdef nosupport}
uses
  SysUtils, Classes, Graphics, Controls, LCLType, WSControls, WSLCLClasses;

function TerminalLoad: Boolean;
function NewTerminal(Control: TTerminalControl): ITerminal;

type
  TWSTerminalControl = class(TWSCustomControl)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;
{$endif}

implementation

{$ifdef nosupport}
uses
  VteIntf;

type
  TTerminal = class(TInterfacedObject, ITerminal)
  private
    FControl: TTerminalControl;
  protected
    procedure SetInfo(Value: Pointer);
    procedure SetColor(Element: TTerminalElement; Value: TColor);
    procedure SetFont(Value: TFont);
    procedure Paint;
    procedure Restart;
  public
    constructor Create(Control: TTerminalControl);
  end;

constructor TTerminal.Create(Control: TTerminalControl);
begin
  inherited Create;
  FControl := Control;
end;

procedure TTerminal.SetInfo(Value: Pointer);
begin
end;

procedure TTerminal.SetColor(Element: TTerminalElement; Value: TColor);
begin
end;

procedure TTerminal.SetFont(Value: TFont);
begin
end;

procedure TTerminal.Paint;
const
  S = 'user@linux~$ bash terminal unsupported';
var
  Canvas: TCanvas;
  W, H: Integer;
begin
  Canvas := FControl.Canvas;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(FControl.ClientRect);
  W := 0; H := 0;
  Canvas.GetTextSize(S, W, H);
  Canvas.Font.Color := Canvas.Pen.Color;
  Canvas.TextOut((FControl.Width -  W) div 2, (FControl.Height -  H) div 2, S);
  Canvas.Pen.Style := psDash;
  Canvas.Pen.Color := clWhite;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(FControl.ClientRect);
end;

procedure TTerminal.Restart;
begin
end;

function TerminalLoad: Boolean;
begin
  Result := False;
end;

function NewTerminal(Control: TTerminalControl): ITerminal;
begin
  Result := TTerminal.Create(Control);
end;


class function TWSTerminalControl.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := 0;
end;
{$endif}

end.

