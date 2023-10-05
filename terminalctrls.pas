unit TerminalCtrls;

{$i terminal.inc}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLIntf;

{ ITerminal }

type
  TTerminalElement = (teFore, teBack, teBold, teDim, teCursor, teHighlight);

  ITerminal = interface
  ['{9A2FEC91-5C43-494C-B6FC-C47742E85316}']
    procedure SetInfo(Value: Pointer);
    procedure SetColor(Element: TTerminalElement; Value: TColor);
    procedure SetFont(Value: TFont);
    procedure Paint;
    procedure Restart;
  end;

{ TTerminalColors }

  TTerminalColors = class(TPersistent)
  private
    FTerminal: ITerminal;
    FColors: array[TTerminalElement] of TColor;
    FChanged: TNotifyEvent;
    procedure SetColor(Index: Integer; Value: TColor);
    function GetColor(Index: Integer): TColor;
  public
    constructor Create(Terminal: ITerminal; Changed: TNotifyEvent);
    procedure Assign(Source: TPersistent); override;
    procedure Restart;
  published
    property Foreground: TColor index 0 read GetColor write SetColor default clSilver;
    property Background: TColor index 1 read GetColor write SetColor default clBlack;
    property Bold: TColor index 2 read GetColor write SetColor default clWhite;
    property Dim: TColor index 3 read GetColor write SetColor default clGray;
    property Cursor: TColor index 4 read GetColor write SetColor default clWhite;
    property Highlight: TColor index 5 read GetColor write SetColor default clWhite;
  end;

{ TTerminalControl }

  TTerminalControl = class(TCustomControl)
  protected
    FTerminal: ITerminal;
  protected
    class procedure WSRegisterClass; override;
    procedure DoReady; virtual;
    procedure DoTerminate; virtual;
    property Terminal: ITerminal read FTerminal;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TCustomTerminal }

  TCustomTerminal = class(TTerminalControl)
  private
    FColors: TTerminalColors;
    FOnReady: TNotifyEvent;
    FOnTerminate: TNotifyEvent;
    procedure SetColors(Value: TTerminalColors);
  protected
    procedure DoReady; override;
    procedure DoTerminate; override;
    procedure ColorsChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject); override;
    procedure Paint; override;
    property Colors: TTerminalColors read FColors write SetColors;
    property OnReady: TNotifyEvent read FOnReady write FOnReady;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Restart;
  end;

{ TTerminal }

  TTerminal = class(TCustomTerminal)
  published
    property Align;
    property Anchors;
    property Colors;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnReady;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnTerminate;
  end;

function TerminalAvaiable: Boolean;

implementation

uses
  LCLType, WSLCLClasses,
{$ifdef terminalgtk2}
  Gtk2Vte;
{$endif}
{$ifdef terminalgtk3}
  Gtk3Vte;
{$endif}
{$ifdef nosupport}
  NoVte;
{$endif}

function TerminalAvaiable: Boolean;
begin
  Result := TerminalLoad;
end;

{ TTerminalColors }

constructor TTerminalColors.Create(Terminal: ITerminal; Changed: TNotifyEvent);
begin
  inherited Create;
  FTerminal := Terminal;
  FColors[teFore] := clSilver;
  FColors[teBack] := clBlack;
  FColors[teBold] := clWhite;
  FColors[teDim] := clGray;
  FColors[teCursor] := clWhite;
  FColors[teHighlight] := clWhite;
  FChanged := Changed;
end;

procedure TTerminalColors.Assign(Source: TPersistent);
var
  C: TTerminalColors;
  E: TTerminalElement;
begin
  if Source = Self then
    Exit;
  if Source is TTerminalColors then
  begin
    C := Source as TTerminalColors;
    for E := Low(FColors) to High(FColors) do
      FColors[E] := C.FColors[E];
    Restart;
    FChanged(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TTerminalColors.Restart;
var
  E: TTerminalElement;
begin
  for E := Low(FColors) to High(FColors) do
    FTerminal.SetColor(E, FColors[E]);
end;

procedure TTerminalColors.SetColor(Index: Integer; Value: TColor);
var
  E: TTerminalElement;
begin
  E := TTerminalElement(Index);
  if Value <> FColors[E] then
  begin
    FColors[E] := Value;
    FTerminal.SetColor(E, FColors[E]);
    FChanged(Self);
  end;
end;

function TTerminalColors.GetColor(Index: Integer): TColor;
var
  E: TTerminalElement;
begin
  E := TTerminalElement(Index);
  Result := FColors[E];
end;

{ TTerminalControl }

var
  Registered: Boolean;

class procedure TTerminalControl.WSRegisterClass;
begin
  if Registered then
    Exit;
  Registered := True;
  if TerminalAvaiable then
    RegisterWSComponent(TTerminalControl, TWSTerminalControl);
end;

constructor TTerminalControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTerminal := NewTerminal(Self);
end;

procedure TTerminalControl.DoReady;
begin
end;

procedure TTerminalControl.DoTerminate;
begin
end;

{ TCustomTerminal }

constructor TCustomTerminal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColors := TTerminalColors.Create(Terminal, ColorsChanged);
  Width := 300;
  Height := 200;
  ParentFont := False;
  Font.Name := 'Monospace';
end;

procedure TCustomTerminal.DoReady;
begin
  FColors.Restart;
  Terminal.SetFont(Font);
  if Assigned(FOnReady) then
    FOnReady(Self);
end;

procedure TCustomTerminal.DoTerminate;
begin
  if Assigned(FOnTerminate) then
    FOnTerminate(Self);
end;

procedure TCustomTerminal.SetColors(Value: TTerminalColors);
begin
  FColors.Assign(Value);
end;

procedure TCustomTerminal.ColorsChanged(Sender: TObject);
begin
  if csDesigning in ComponentState then
    Invalidate;
end;

procedure TCustomTerminal.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  Terminal.SetFont(Font);
  if csDesigning in ComponentState then
    Invalidate;
end;

procedure TCustomTerminal.Paint;
begin
  Canvas.Brush.Color := FColors.Background;
  Canvas.Pen.Color := FColors.Foreground;
  Canvas.Font.Assign(Font);
  Terminal.Paint;
end;

procedure TCustomTerminal.Restart;
begin
  Terminal.Restart;
end;

end.

