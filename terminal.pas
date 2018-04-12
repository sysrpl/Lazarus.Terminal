{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit terminal;

{$warn 5023 off : no warning about unused units}
interface

uses
  Gtk2Term, TerminalCtrls, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('terminal', @Register);
end.
