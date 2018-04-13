unit VteIntf;

{$mode delphi}

interface

uses
	Classes, SysUtils, Graphics, Controls;

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

{ TTerminalControl }

  TTerminalControl = class(TCustomControl)
	private
    FTerminal: ITerminal;
    FOnTerminate: TNotifyEvent;
	protected
    procedure DoTerminate; virtual;
		property Terminal: ITerminal read FTerminal;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
	public
    constructor Create(AOwner: TComponent); override;
	end;

function TerminalAvaiable: Boolean;

implementation

{$ifdef lclgtk2}
uses
  Vte, LCLType, WSControls, WSLCLClasses, GLib2, Gtk2, Gtk2Def, Gtk2Proc,
  Pango, Gtk2WSControls;

function TerminalAvaiable: Boolean;
begin
  Result := TerminalLoad;
end;

procedure TerminalExit(Widget: PGtkWidget); cdecl;
var
  Info: PWidgetInfo;
begin
  Info := PWidgetInfo(g_object_get_data(PGObject(Widget), 'widgetinfo'));
  TTerminalControl(Info.LCLObject).DoTerminate;
end;

{$region Actual ITerminal interface}
{ TTerminal }

type
  TTerminal = class(TInterfacedObject, ITerminal)
	private
    FControl: TTerminalControl;
    FInfo: PWidgetInfo;
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
  if FInfo = nil then
		FInfo := Value;
end;

type
  TGdkColor = packed record
    pixel: LongWord;
    red, green, blue: Word;
	end;

procedure ColorToGdk(C: TColor; out G: TGdkColor);
begin
  C := ColorToRGB(C);
  G.pixel := 0;
  G.red := (C and $FF) * $FF;
  G.green := (C shr 8 and $FF) * $FF;
  G.blue := (C shr 16 and $FF) * $FF;
end;

{ How to set a Gtk object property:
var
	V: TGValue;
begin
	V.g_type := G_TYPE_DOUBLE;
  V.data[0].v_double := 3;
  g_object_set_property(PGObject(Widget), 'scale', @V);
end; }

procedure TTerminal.SetColor(Element: TTerminalElement; Value: TColor);
var
  C: TGdkColor;
begin
	if FInfo = nil then
    Exit;
  ColorToGdk(Value, C);
  case Element of
  	teFore: vte_terminal_set_color_foreground(VTE_TERMINAL(FInfo.ClientWidget), @C);
  	teBack: vte_terminal_set_color_background(VTE_TERMINAL(FInfo.ClientWidget), @C);
  	teBold: vte_terminal_set_color_bold(VTE_TERMINAL(FInfo.ClientWidget), @C);
  	teDim: vte_terminal_set_color_dim(VTE_TERMINAL(FInfo.ClientWidget), @C);
  	// teCursor: vte_terminal_set_color_cursor(VTE_TERMINAL(FInfo.ClientWidget), @C);
  	teHighlight: vte_terminal_set_color_highlight(VTE_TERMINAL(FInfo.ClientWidget), @C);
  end;
end;

procedure TTerminal.SetFont(Value: TFont);
const
  Weights: array[Boolean] of TPangoWeight =
    (PANGO_WEIGHT_NORMAL, PANGO_WEIGHT_BOLD);
  Styles: array[Boolean] of TPangoStyle =
    (PANGO_STYLE_NORMAL, PANGO_STYLE_ITALIC);
var
  Name: string;
  Size: Integer;
  F: PPangoFontDescription;
  P: PChar;
  S, T: string;
  I: Integer;
begin
	if FInfo = nil then
    Exit;
  Name := LowerCase(Value.Name);
  Size := Value.Size;
  if (Name = 'default') or (Size < 1) then
	begin
  	g_object_get(gtk_settings_get_default, 'gtk-font-name', [@P, nil]);
    S := P;
    if Name = 'default' then
    begin
			I := Length(S);
      while S[I] in ['0'..'9'] do
			begin
        S[I] := ' ';
        Dec(I);
			end;
      Name := Trim(S);
		end
    else
    	Name := Value.Name;
    S := P;
    if Size = 0 then
    begin
			I := Length(S);
      T := '';
      while S[I] in ['0'..'9'] do
			begin
        T := S[I] + T;
        Dec(I);
			end;
      Size := StrToInt(T);
		end;
		g_free(P);
	end;
  F := pango_font_description_new;
	pango_font_description_set_family(F, PChar(Name));
  pango_font_description_set_weight(F, Weights[fsBold in Value.Style]);
  pango_font_description_set_style(F, Styles[fsItalic in Value.Style]);
	pango_font_description_set_size(F, Round(Size * PANGO_SCALE));
	vte_terminal_set_font(VTE_TERMINAL(FInfo.ClientWidget), F);
  pango_font_description_free(F);
end;

procedure TTerminal.Paint;
const
  S = 'user@linux~$ bash terminal';
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
var
  Args: array[0..1] of PChar = ('/bin/bash', nil);
begin
  if FInfo = nil then
    Exit;
  gtk_widget_destroy(FInfo.ClientWidget);
  FInfo.ClientWidget := vte_terminal_new;
  vte_terminal_set_allow_bold(VTE_TERMINAL(FInfo.ClientWidget), True);
  vte_terminal_fork_command_full(VTE_TERMINAL(FInfo.ClientWidget), VTE_PTY_DEFAULT,
    nil, @Args[0], nil, G_SPAWN_SEARCH_PATH, nil, nil, nil, nil);
  gtk_container_add(GTK_CONTAINER(FInfo.CoreWidget), FInfo.ClientWidget);
  g_object_set_data(PGObject(FInfo.ClientWidget), 'widgetinfo', FInfo);
  gtk_widget_show_all(FInfo.CoreWidget);
  g_signal_connect(FInfo.ClientWidget, 'child-exited', G_CALLBACK(@TerminalExit), nil);
end;

function NewTerminal(Control: TTerminalControl): ITerminal;
begin
	Result := TTerminal.Create(Control);
end;
{$endregion}

{$region TGtk2WSTerminalControl}
{ TGtk2WSTerminalControl }

type
  TGtk2WSTerminalControl = class(TWSCustomControl)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

class procedure TGtk2WSTerminalControl.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
end;

class function TGtk2WSTerminalControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Info: PWidgetInfo;
  Style: PGtkRCStyle;
  Args: array[0..1] of PChar = ('/bin/bash', nil);
  Allocation: TGTKAllocation;
begin
  { Initialize widget info }
  Info := CreateWidgetInfo(gtk_frame_new(nil), AWinControl, AParams);
  Info.LCLObject := AWinControl;
  Info.Style := AParams.Style;
  Info.ExStyle := AParams.ExStyle;
  Info.WndProc := {%H-}PtrUInt(AParams.WindowClass.lpfnWndProc);
  { Configure core and client }
  gtk_frame_set_shadow_type(PGtkFrame(Info.CoreWidget), GTK_SHADOW_NONE);
  Style := gtk_widget_get_modifier_style(Info.CoreWidget);
  Style.xthickness := 0;
  Style.ythickness := 0;
  gtk_widget_modify_style(Info.CoreWidget, Style);
  if csDesigning in AWinControl.ComponentState then
    Info.ClientWidget := CreateFixedClientWidget(True)
  else
  begin
    Info.ClientWidget := vte_terminal_new;
    vte_terminal_set_allow_bold(VTE_TERMINAL(Info.ClientWidget), True);
    vte_terminal_fork_command_full(VTE_TERMINAL(Info.ClientWidget), VTE_PTY_DEFAULT,
      nil, @Args[0], nil, G_SPAWN_SEARCH_PATH, nil, nil, nil, nil);
    TTerminalControl(AWinControl).FTerminal.SetInfo(Info);
  end;
  GTK_WIDGET_SET_FLAGS(Info.CoreWidget, GTK_CAN_FOCUS);
  gtk_container_add(GTK_CONTAINER(Info.CoreWidget), Info.ClientWidget);
  g_object_set_data(PGObject(Info.ClientWidget), 'widgetinfo', Info);
  gtk_widget_show_all(Info.CoreWidget);
  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(Info.CoreWidget, @Allocation);
  g_signal_connect(Info.ClientWidget, 'child-exited', G_CALLBACK(@TerminalExit), nil);
  SetCallbacks(Info.CoreWidget, Info);
  Result := {%H-}TLCLIntfHandle(Info.CoreWidget);
end;
{$endregion}

procedure TerminalRegister;
begin
  if TerminalAvaiable then
    RegisterWSComponent(TTerminalControl, TGtk2WSTerminalControl);
end;
{$else}
function TerminalAvaiable: Boolean;
begin
  Result := False;
end;

{$region Dummy ITerminal interface}
{ TTerminal }

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
  S = 'user@linux~$ bash terminal';
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

function NewTerminal(Control: TTerminalControl): ITerminal;
begin
	Result := TTerminal.Create(Control);
end;

procedure TerminalRegister;
begin
end;
{$endif}

constructor TTerminalControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTerminal := TTerminal.Create(Self);
end;

procedure TTerminalControl.DoTerminate;
begin
  if Assigned(FOnTerminate) then
		FOnTerminate(Self);
end;

initialization
  TerminalRegister;
end.

