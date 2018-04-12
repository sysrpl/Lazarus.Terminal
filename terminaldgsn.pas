{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit terminaldgsn;

{$warn 5023 off : no warning about unused units}
interface

uses
  TerminalRegister, TerminalEmulatorDlg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('TerminalRegister', @TerminalRegister.Register);
end;

initialization
  RegisterPackage('terminaldgsn', @Register);
end.
