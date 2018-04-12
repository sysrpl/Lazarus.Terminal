unit TerminalRegister;

{$mode delphi}

interface

uses
  Classes, LazIDEIntf, ProjectIntf, MenuIntf, TerminalCtrls, TerminalEmulatorDlg;

procedure Register;

implementation

{$R terminal_icons.res}

procedure Register;
begin
  {$ifdef lclgtk2}
  RegisterComponents('Additional', [TTerminal]);
  RegisterIDEMenuCommand(itmViewSecondaryWindows, 'TerminalEmulatorItem', 'Terminal',
    nil, ShowTerminalEmulatorDialog, nil, 'menu_information');
  InitTerminalEmulatorDialog;
  {$endif}
end;

end.
