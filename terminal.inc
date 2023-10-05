{$mode delphi}
{$define nosupport}
{$ifdef linux}
  {$ifdef lclgtk2}
    {$undef nosupport}
    {$define terminalgtk2}
  {$endif}
  {$ifdef lclgtk3}
    {$undef nosupport}
    {$define terminalgtk3}
  {$endif}
{$endif}
