unit TerminalCtrls;

{$mode delphi}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, LCLIntf, LMessages, VteIntf;

{ TTerminalColors }

type
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

{ TCustomTerminal }

  TCustomTerminal = class(TTerminalControl)
  private
    FColors: TTerminalColors;
    procedure WMTimer(var Message: TLMTimer); message LM_TIMER;
    procedure SetColors(Value: TTerminalColors);
  protected
    procedure CreateHandle; override;
    procedure ColorsChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject); override;
    procedure Paint; override;
    property Colors: TTerminalColors read FColors write SetColors;
    procedure Loaded; override;
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
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnTerminate;
  end;

function TerminalAvaiable: Boolean;

implementation

function TerminalAvaiable: Boolean;
begin
  Result := VteIntf.TerminalAvaiable;
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

{ TCustomTerminal }

const
  HandleCreationTimer = $100;

constructor TCustomTerminal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColors := TTerminalColors.Create(Terminal, ColorsChanged);
  Width := 300;
  Height := 200;
  ParentFont := False;
  Font.Name := 'Monospace';
end;

procedure TCustomTerminal.WMTimer(var Message: TLMTimer);
begin
  KillTimer(Handle, HandleCreationTimer);
  FColors.Restart;
  Terminal.SetFont(Font);
end;

procedure TCustomTerminal.CreateHandle;
begin
  inherited CreateHandle;
  SetTimer(Handle, HandleCreationTimer, 1, nil);
end;

procedure TCustomTerminal.Loaded;
begin
  inherited Loaded;
  FColors.Restart;
  Terminal.SetFont(Font);
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
  FColors.Restart;
  Terminal.SetFont(Font);
end;

end.

