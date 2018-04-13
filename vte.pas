unit Vte;

{$mode delphi}

interface

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

function TerminalLoad: Boolean;

implementation

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

end.
