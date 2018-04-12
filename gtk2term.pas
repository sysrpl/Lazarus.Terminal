unit Gtk2Term;

{$mode delphi}

interface

{$ifdef lclgtk2}
uses
  GLib2, Gtk2, dynlibs, gdk2, pango;

type
  GPid = LongWord;
  PGPid = ^GPid;
  GError = LongWord;
  PGError = ^GError;

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
    widget: PGtkWidget;
  end;

  VTE_TERMINAL = PVteTerminal;

var
  vte_terminal_new: function: PGtkWidget; cdecl;

  vte_terminal_fork_command_full: function(terminal: PVteTerminal; pty_flags: TVtePtyFlags;
    working_directory: PChar; argv, envv: PPChar; spawn_flags: TGSpawnFlags;
    child_setup: TGSpawnChildSetupFunc; child_setup_data: Pointer; child_pid:
    PGPid; error: PGError): GBoolean; cdecl;

    vte_terminal_set_color_background: procedure(terminal: PVteTerminal;
      const background: PGdkColor); cdecl;

    vte_terminal_set_color_foreground: procedure(terminal: PVteTerminal;
      const background: PGdkColor); cdecl;

    vte_terminal_set_color_highlight: procedure(terminal: PVteTerminal;
      const background: PGdkColor); cdecl;

    vte_terminal_set_color_highlight_foreground: procedure(terminal: PVteTerminal;
      const background: PGdkColor); cdecl;

    vte_terminal_set_font: procedure (terminal: PVteTerminal;
      const font_desc: PPangoFontDescription); cdecl;

function Gtk2TermLoad: Boolean;

implementation

var
  Initialized: Boolean;
  Loaded: Boolean;

function Gtk2TermLoad: Boolean;
const
  vte = 'libvte.so';
var
  Lib: TLibHandle;
begin
  if Initialized then
    Exit(Loaded);
  Initialized := True;
  Lib := LoadLibrary(vte);
  if Lib = 0 then
    Exit(Loaded);

  @vte_terminal_new := GetProcAddress(Lib, 'vte_terminal_new');
  @vte_terminal_fork_command_full := GetProcAddress(Lib, 'vte_terminal_fork_command_full');
  @vte_terminal_set_color_background := GetProcAddress(Lib, 'vte_terminal_set_color_background');
  @vte_terminal_set_color_foreground := GetProcAddress(Lib, 'vte_terminal_set_color_foreground');
  @vte_terminal_set_color_highlight := GetProcAddress(Lib, 'vte_terminal_set_color_highlight');
  @vte_terminal_set_color_highlight_foreground := GetProcAddress(Lib, 'vte_terminal_set_color_highlight_foreground');
  @vte_terminal_set_font := GetProcAddress(Lib, 'vte_terminal_set_font');

  Loaded :=
    (@vte_terminal_new <> nil) and
    (@vte_terminal_fork_command_full <> nil);
  Result := Loaded;
end;
{$else}
implementation
{$endif}

end.
