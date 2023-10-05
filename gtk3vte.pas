unit Gtk3Vte;

{$i terminal.inc}

interface

{$ifdef linux}
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

{$ifdef linux}

uses
  // Pango,
  LazGtk3, LazGdk3, LazGObject2, LazGLib2, LazCairo1, LazPango1, LazGdkPixbuf2,
  Gtk3objects, Gtk3procs, Gtk3WSControls, Gtk3Widgets;

type
  guint8 = Byte;
  guint16 = Word;
  guint32 = LongWord;

  GPid = LongWord;
  PGPid = ^GPid;
  GError = LongWord;
  PGError = ^GError;
  GCancellable = Pointer;
  PGCancellable = ^GCancellable;
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
  vte_terminal_spawn_sync: function(terminal: PVteTerminal; pty_flags: TVtePtyFlags;
    working_directory: PChar; argv, envv: PPChar; spawn_flags: TGSpawnFlags;
    child_setup: TGSpawnChildSetupFunc; child_setup_data: Pointer; child_pid:
    PGPid; cancellable: PGCancellable; error: PGError): GBoolean; cdecl;
  vte_terminal_set_allow_bold: procedure(terminal: PVteTerminal; allow_bold: GBoolean); cdecl;
  vte_terminal_set_font: procedure(terminal: PVteTerminal; font_desc: PPangoFontDescription); cdecl;
  vte_terminal_set_color_foreground: procedure(terminal: PVteTerminal; foreground: PGdkColor); cdecl;
  vte_terminal_set_color_background: procedure(terminal: PVteTerminal; background: PGdkColor); cdecl;
  vte_terminal_set_color_bold: procedure(terminal: PVteTerminal; foreground: PGdkColor); cdecl;
  vte_terminal_set_color_cursor: procedure(terminal: PVteTerminal; background: PGdkColor); cdecl;
  vte_terminal_set_color_highlight: procedure(terminal: PVteTerminal; background: PGdkColor); cdecl;

var
  Initialized: Boolean;
  Loaded: Boolean;

function TerminalLoad: Boolean;
const
  vte = 'libvte-2.91.so.0';
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
    Load('vte_terminal_spawn_sync', @vte_terminal_spawn_sync) and
    Load('vte_terminal_set_allow_bold', @vte_terminal_set_allow_bold) and
    Load('vte_terminal_set_font', @vte_terminal_set_font) and
    Load('vte_terminal_set_color_foreground', @vte_terminal_set_color_foreground) and
    Load('vte_terminal_set_color_background', @vte_terminal_set_color_background) and
    Load('vte_terminal_set_color_bold', @vte_terminal_set_color_bold) and
    Load('vte_terminal_set_color_cursor', @vte_terminal_set_color_cursor) and
    Load('vte_terminal_set_color_highlight', @vte_terminal_set_color_highlight);
  Loaded := False;
  Result := Loaded;
end;

type
  TTerminalHack = class(TTerminalControl)
  end;

procedure TerminalReady(Widget: PGtkWidget); cdecl;
begin
end;

procedure TerminalExit(Widget: PGtkWidget); cdecl;
begin
end;

type
  TGtk3Terminal = class(TGtk3Widget)
  private
    FTerminal: PVteTerminal;
  public
    constructor Create(const AWinControl: TWinControl;
      const AParams: TCreateParams); override;
    procedure DoReady;
    procedure DoExit;
    property Terminal: PVteTerminal read FTerminal write FTerminal;
  end;

//   g_signal_connect_data(FWidget, 'destroy', TGCallback(@TGtk3Widget.destroy_event), Self, nil, 0);

constructor TGtk3Terminal.Create(const AWinControl: TWinControl;
  const AParams: TCreateParams);
var
  Frame: PGtkFrame;
  Teminal: PVteTerminal;
begin
  inherited Create(AWinControl, AParams);
  Frame := gtk_frame_new(nil);
  gtk_frame_set_shadow_type(Frame, GTK_SHADOW_NONE);
  if

  Self.Widget := Frame;

  g_signal_connect_data();
  //  g_signal_connect_data(FWidget, 'destroy', TGCallback(@TGtk3Widget.destroy_event), Self, nil, 0);

end;


procedure TGtk3Terminal.DoReady;
begin
end;

procedure TGtk3Terminal.DoExit;
begin
end;


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
  S = 'user@linux~$ bash terminal Gtk3';
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

{ TWSTerminalControl }

class function TWSTerminalControl.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  T: TGtk3Terminal;
begin
  T := TGtk3Terminal.Create(AWinControl, AParams);
  T.Widget := ;
  Result := 0;
end;
{$endif}

end.

