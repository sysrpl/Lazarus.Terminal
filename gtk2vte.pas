unit Gtk2Vte;

{$i terminal.inc}

interface

{$ifdef terminalgtk2}
uses
  SysUtils, Classes, Graphics, Controls, LCLType, WSControls, WSLCLClasses,
  TerminalCtrls;

function TerminalLoad: Boolean;
function NewTerminal(Control: TTerminalControl): ITerminal;

type
  TWSTerminalControl = class(TWSCustomControl)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;
{$endif}

implementation

{$ifdef terminalgtk2}
uses
  GLib2, Gtk2, Gtk2Def, Gtk2Proc, Pango, Gtk2WSControls;

type
  guint8 = Byte;
  guint16 = Word;
  guint32 = LongWord;

  GPid = LongWord;
  PGPid = ^GPid;
  GError = LongWord;
  PGError = ^GError;
  PGtkWidget = Pointer;
  GBoolean = LongBool;

  PGdkColor = Pointer;
  PPangoFontDescription = Pointer;

  TVtePtyFlags = LongWord;

const
  VTE_PTY_DEFAULT = $0;
  VTE_PTY_NO_LASTLOG = $1;
  VTE_PTY_NO_UTMP = $2;
  VTE_PTY_NO_WTMP = $4;
  VTE_PTY_NO_HELPER = $8;
  VTE_PTY_NO_FALLBACK = $10;

type
  TGSpawnFlags = LongWord;

const
  G_SPAWN_DEFAULT = $0;
  G_SPAWN_LEAVE_DESCRIPTORS_OPEN = $1;
  G_SPAWN_DO_NOT_REAP_CHILD = $2;
  G_SPAWN_SEARCH_PATH = $4;
  G_SPAWN_STDOUT_TO_DEV_NULL = $8;
  G_SPAWN_STDERR_TO_DEV_NULL = $10;
  G_SPAWN_CHILD_INHERITS_STDIN = $20;
  G_SPAWN_FILE_AND_ARGV_ZERO = $40;
  G_SPAWN_SEARCH_PATH_FROM_ENVP = $80;
  G_SPAWN_CLOEXEC_PIPES = $100;

type
  TGSpawnChildSetupFunc = procedure(user_data: Pointer); cdecl;

  PVteTerminal = ^TVteTerminal;
  TVteTerminal = record
  end;

  VTE_TERMINAL = PVteTerminal;

var
  vte_terminal_new: function: PGtkWidget; cdecl;
  vte_terminal_fork_command_full: function(terminal: PVteTerminal; pty_flags: TVtePtyFlags;
    working_directory: PChar; argv, envv: PPChar; spawn_flags: TGSpawnFlags;
    child_setup: TGSpawnChildSetupFunc; child_setup_data: Pointer; child_pid:
    PGPid; error: PGError): GBoolean; cdecl;
  vte_terminal_set_allow_bold: procedure(terminal: PVteTerminal; allow_bold: GBoolean); cdecl;
  vte_terminal_set_font: procedure(terminal: PVteTerminal; font_desc: PPangoFontDescription); cdecl;
  vte_terminal_set_color_foreground: procedure(terminal: PVteTerminal; foreground: PGdkColor); cdecl;
  vte_terminal_set_color_background: procedure(terminal: PVteTerminal; background: PGdkColor); cdecl;
  vte_terminal_set_color_bold: procedure(terminal: PVteTerminal; foreground: PGdkColor); cdecl;
  vte_terminal_set_color_dim: procedure(terminal: PVteTerminal; background: PGdkColor); cdecl;
  vte_terminal_set_color_cursor: procedure(terminal: PVteTerminal; background: PGdkColor); cdecl;
  vte_terminal_set_color_highlight: procedure(terminal: PVteTerminal; background: PGdkColor); cdecl;

var
  Initialized: Boolean;
  Loaded: Boolean;

function TerminalLoad: Boolean;
const
  vte = 'libvte.so.9';
var
  Lib: TLibHandle;

  function Load(const ProcName : string; out Proc: Pointer): Boolean;
  begin
    Proc := GetProcAddress(Lib, ProcName);
    Result := Proc <> nil;
  end;

begin
  if Initialized then
    Exit(Loaded);
  Initialized := True;
  Lib := LoadLibrary(vte);
  if Lib = 0 then
    Exit(Loaded);
  Loaded :=
    Load('vte_terminal_new', @vte_terminal_new) and
    Load('vte_terminal_fork_command_full', @vte_terminal_fork_command_full) and
    Load('vte_terminal_set_allow_bold', @vte_terminal_set_allow_bold) and
    Load('vte_terminal_set_font', @vte_terminal_set_font) and
    Load('vte_terminal_set_color_foreground', @vte_terminal_set_color_foreground) and
    Load('vte_terminal_set_color_background', @vte_terminal_set_color_background) and
    Load('vte_terminal_set_color_bold', @vte_terminal_set_color_bold) and
    Load('vte_terminal_set_color_dim', @vte_terminal_set_color_dim) and
    Load('vte_terminal_set_color_cursor', @vte_terminal_set_color_cursor) and
    Load('vte_terminal_set_color_highlight', @vte_terminal_set_color_highlight);
  Result := Loaded;
end;

function TerminalAvailable: Boolean;
begin
  Result := TerminalLoad;
end;

type
  TTerminalHack = class(TTerminalControl)
  end;


procedure TerminalReady(Widget: PGtkWidget); cdecl;
var
  Info: PWidgetInfo;
begin
  g_signal_handlers_disconnect_by_func(Widget, @TerminalReady, nil);
  Info := PWidgetInfo(g_object_get_data(PGObject(Widget), 'widgetinfo'));
  TTerminalHack(Info.LCLObject).DoReady;
end;

procedure TerminalExit(Widget: PGtkWidget); cdecl;
var
  Info: PWidgetInfo;
begin
  Info := PWidgetInfo(g_object_get_data(PGObject(Widget), 'widgetinfo'));
  TTerminalHack(Info.LCLObject).DoTerminate;
end;

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
  {%H-}case Element of
    teFore: vte_terminal_set_color_foreground(VTE_TERMINAL(FInfo.ClientWidget), @C);
    teBack: vte_terminal_set_color_background(VTE_TERMINAL(FInfo.ClientWidget), @C);
    teBold: vte_terminal_set_color_bold(VTE_TERMINAL(FInfo.ClientWidget), @C);
    teDim: vte_terminal_set_color_dim(VTE_TERMINAL(FInfo.ClientWidget), @C);
    { For some reason setting the cursor color causes it to be removed entirely

    teCursor: vte_terminal_set_color_cursor(VTE_TERMINAL(FInfo.ClientWidget), @C); }
    teHighlight: vte_terminal_set_color_highlight(VTE_TERMINAL(FInfo.ClientWidget), @C);
  end;
end;

procedure TTerminal.SetFont(Value: TFont);
var
  F: PPangoFontDescription;
begin
  if FInfo = nil then
    Exit;
  F :=  pango_layout_get_font_description({%H-}PGDIObject(Value.Handle).GDIFontObject);
  vte_terminal_set_font(VTE_TERMINAL(FInfo.ClientWidget), F);
end;

procedure TTerminal.Paint;
const
  S = 'user@linux~$ bash terminal gtk2';
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
  g_signal_connect(FInfo.ClientWidget, 'contents-changed', G_CALLBACK(@TerminalReady), nil);
  g_signal_connect(FInfo.ClientWidget, 'child-exited', G_CALLBACK(@TerminalExit), nil);
end;

function NewTerminal(Control: TTerminalControl): ITerminal;
begin
  Result := TTerminal.Create(Control);
end;

procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
end;

class function TWSTerminalControl.CreateHandle(const AWinControl: TWinControl;
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
    g_signal_connect(Info.ClientWidget, 'contents-changed', G_CALLBACK(@TerminalReady), nil);
    g_signal_connect(Info.ClientWidget, 'child-exited', G_CALLBACK(@TerminalExit), nil);
    vte_terminal_set_allow_bold(VTE_TERMINAL(Info.ClientWidget), True);
    vte_terminal_fork_command_full(VTE_TERMINAL(Info.ClientWidget), VTE_PTY_DEFAULT,
      nil, @Args[0], nil, G_SPAWN_SEARCH_PATH, nil, nil, nil, nil);
    TTerminalHack(AWinControl).FTerminal.SetInfo(Info);
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
  SetCallbacks(Info.CoreWidget, Info);
  Result := {%H-}TLCLIntfHandle(Info.CoreWidget);
end;
{$endif}

end.
