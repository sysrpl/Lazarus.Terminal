unit TerminalCtrls;

{$mode delphi}

{$ifdef lclgtk2}
  {$ifdef unix}
    {$define hasgtk2term}
  {$endif}
{$endif}

interface

uses
  Gtk2Term, Classes, SysUtils, Controls, Graphics;

type

  TTerminal = class(TCustomControl)
  private
    FInfo: Pointer;
    fTerminalHanlde: PVteTerminal;
    fOnTerminate: TNotifyEvent;
    fOnTerminalVisibleChanged: TNotifyEvent;
    fBackgroundColor: TColor;
    fForegroundColor: TColor;
    fSelectedColor: TColor;
    procedure setBackgroundColor(value: TColor);
    procedure setForegroundColor(value: TColor);
    procedure setSelectedColor(value: TColor);
  protected
    // Only used at design-time.
    procedure Paint; override;
    procedure DoTerminate; virtual;
    procedure DoTerminalVisibleChanged; virtual;
    procedure FontChanged(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Restart;
    // Sends a command, as it would be manually typed. Line feed is automatically added.
    procedure Command(const data: string);
  published
    // Background color
    property backgroundColor: TColor read fBackgroundColor write setBackgroundColor default clBlack;
    // Font color
    property foregroundColor: TColor read fForegroundColor write setForegroundColor default clWhite;
    // Background color for the selection
    property selectedColor: TColor read fSelectedColor write setSelectedColor default clWhite;
    property Align;
    property Anchors;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DoubleBuffered;
    property Enabled;
    // The name and height properties are handled. see foregroundColor for Color.
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
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
    // Note: The hosted widget is there and visual settings can be applied.
    // In many cases DoFirstShow, OnShow and likes will happen too quickly.
    property OnTerminalVisibleChanged: TNotifyEvent read fOnTerminalVisibleChanged write fOnTerminalVisibleChanged;
  end;

function TerminalAvailable: Boolean;

implementation

{$ifdef hasgtk2term}
uses
  LCLType, WSControls, WSLCLClasses, GLib2, Gtk2, Gtk2Def, Gtk2Proc,
  Gtk2WSControls, gdk2;

type

  TGtk2WSTerminal = class(TWSCustomControl)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget;
      const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const AWinControl: TWinControl;
        const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

procedure TerminalExit(Widget: PGtkWidget); cdecl;
var
  Info: PWidgetInfo;
begin
  Info := PWidgetInfo(g_object_get_data(PGObject(Widget), 'widgetinfo'));
  TTerminal(Info.LCLObject).DoTerminate;
end;

procedure TerminalRefresh(Widget: PGtkWidget); cdecl;
var
  Info: PWidgetInfo;
begin
  Info := PWidgetInfo(g_object_get_data(PGObject(Widget), 'widgetinfo'));
  TTerminal(Info.LCLObject).DoTerminalVisibleChanged;
end;

class procedure TGtk2WSTerminal.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
end;

class function TGtk2WSTerminal.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Info: PWidgetInfo;
  Style: PGtkRCStyle;
  Args: array[0..1] of PChar = (nil, nil);
  Allocation: TGTKAllocation;
const
  Flgs: array[boolean] of integer = (GTK_CAN_FOCUS, GTK_CAN_FOCUS or GTK_DOUBLE_BUFFERED);
begin
  Args[0] := vte_get_user_shell();
  if Args[0] = nil then
    Args[0] := '/bin/bash';

  { Initialize widget info }
  Info := CreateWidgetInfo(gtk_frame_new(nil), AWinControl, AParams);
  Info.LCLObject := AWinControl;
  Info.Style := AParams.Style;
  Info.ExStyle := AParams.ExStyle;
  Info.WndProc := {%H-}PtrUInt(AParams.WindowClass.lpfnWndProc);
  TTerminal(AWinControl).FInfo := Info;

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
    Info.ClientWidget := vte_terminal_new();
    TTerminal(AWinControl).fTerminalHanlde := VTE_TERMINAL(Info.ClientWidget);
    vte_terminal_fork_command_full(VTE_TERMINAL(Info.ClientWidget), VTE_PTY_DEFAULT,
      nil, @Args[0], nil, G_SPAWN_SEARCH_PATH, nil, nil, nil, nil);
  end;
  GTK_WIDGET_SET_FLAGS(Info.CoreWidget, Flgs[AWinControl.DoubleBuffered]);
  gtk_container_add(GTK_CONTAINER(Info.CoreWidget), Info.ClientWidget);
  g_object_set_data(PGObject(Info.ClientWidget), 'widgetinfo', Info);
  gtk_widget_show_all(Info.CoreWidget);
  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(Info.CoreWidget, @Allocation);
  g_signal_connect(Info.ClientWidget, 'child-exited', G_CALLBACK(@TerminalExit), nil);
  g_signal_connect(Info.ClientWidget, 'contents-changed', G_CALLBACK(@TerminalRefresh), nil);
  SetCallbacks(Info.CoreWidget, Info);
  Result := {%H-}TLCLIntfHandle(Info.CoreWidget);
end;
{$endif}

procedure TTerminal.DoTerminate;
begin
  if Assigned(FOnTerminate) then
    FOnTerminate(Self);
end;

procedure TTerminal.DoTerminalVisibleChanged;
begin
  if Assigned(fOnTerminalVisibleChanged) then
    fOnTerminalVisibleChanged(Self);
end;

procedure TTerminal.Restart;
{$ifdef hasgtk2term}
var
  Info: PWidgetInfo;
  Args: array[0..1] of PChar = (nil, nil);
{$endif}
begin
  {$ifdef hasgtk2term}
  if not HandleAllocated then
    Exit;

  Args[0] := vte_get_user_shell();
  if Args[0] = nil then
    Args[0] := '/bin/bash';
  Info := PWidgetInfo(FInfo);
  gtk_widget_destroy(Info.ClientWidget);
  Info.ClientWidget := vte_terminal_new;
  fTerminalHanlde := VTE_TERMINAL(Info.ClientWidget);
  vte_terminal_fork_command_full(fTerminalHanlde, VTE_PTY_DEFAULT,
    nil, @Args[0], nil, G_SPAWN_SEARCH_PATH, nil, nil, nil, nil);
  gtk_container_add(GTK_CONTAINER(Info.CoreWidget), Info.ClientWidget);
  g_object_set_data(PGObject(Info.ClientWidget), 'widgetinfo', Info);
  gtk_widget_show_all(Info.CoreWidget);
  g_signal_connect(Info.ClientWidget, 'child-exited', G_CALLBACK(@TerminalExit), nil);
  g_signal_connect(Info.ClientWidget, 'contents-changed', G_CALLBACK(@TerminalRefresh), nil);
  {$endif}
end;

procedure TTerminal.Command(const data: string);
begin
  {$ifdef hasgtk2term}
  if assigned(fTerminalHanlde) and assigned(vte_terminal_feed_child) then
    vte_terminal_feed_child(fTerminalHanlde, PChar(data + #10), data.Length + 1);
  {$endif}
end;

function TerminalAvailable: Boolean;
begin
  {$ifdef hasgtk2term}
  Result := Gtk2TermLoad;
  {$else}
  Result := false;
  {$endif}
end;

function RegisterTerminal: Boolean;
begin
  Result := TerminalAvailable;
  if Result then
    RegisterWSComponent(TTerminal, TGtk2WSTerminal);
end;

constructor TTerminal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 300;
  Height := 200;
  fBackgroundColor:= clBlack;
  fForegroundColor:= clWhite;
  fSelectedColor:= clWhite;
  Font.Height:=11;
  Font.Name:='Monospace';
end;

procedure TTerminal.Paint;
const
  s = 'linux@user:~$ bash terminal';
var
  w: integer = 0;
  h: integer = 0;
begin
  if not (csDesigning in ComponentState) then
    exit;

  Canvas.Pen.Style := psDash;
  Canvas.Pen.Color := clWhite;
  Canvas.Brush.Color := clBlack;
  Canvas.Font.Color := clWhite;
  Canvas.FillRect(ClientRect);
  Canvas.Rectangle(ClientRect);
  Canvas.GetTextSize(s, w, h);
  Canvas.TextOut((Width -  w) div 2, (Height -  h) div 2, s);
end;

procedure TTerminal.setBackgroundColor(value: TColor);
var
  c: TGDKColor;
begin
  fBackgroundColor:=value;
  {$ifdef hasgtk2term}
  if assigned(fTerminalHanlde) and assigned(vte_terminal_set_color_background) then
  begin
    c := TColortoTGDKColor(fBackgroundColor);
    vte_terminal_set_color_background(fTerminalHanlde, @c);
  end;
  {$endif}
end;

procedure TTerminal.setForegroundColor(value: TColor);
var
  c: TGDKColor;
begin
  fForegroundColor:=value;
  {$ifdef hasgtk2term}
  if assigned(fTerminalHanlde) and assigned(vte_terminal_set_color_foreground) then
  begin
    c := TColortoTGDKColor(fForegroundColor);
    vte_terminal_set_color_foreground(fTerminalHanlde, @c);
  end;
  {$endif}
end;

procedure TTerminal.setSelectedColor(value: TColor);
var
  c: TGDKColor;
begin
  fSelectedColor:=value;
  {$ifdef hasgtk2term}
  if assigned(fTerminalHanlde) and assigned(vte_terminal_set_color_highlight)
    and assigned(vte_terminal_set_color_highlight_foreground) then
  begin
    c := TColortoTGDKColor(fSelectedColor);
    vte_terminal_set_color_highlight(fTerminalHanlde, @c);
    c := TColortoTGDKColor(InvertColor(fSelectedColor));
    vte_terminal_set_color_highlight_foreground(fTerminalHanlde, @c);
  end;
  {$endif}
end;

procedure TTerminal.FontChanged(Sender: TObject);
begin
  inherited;
  {$ifdef hasgtk2term}
  {$push}{$Hints off}
  if assigned(fTerminalHanlde) and assigned(vte_terminal_set_font) and
    (Handle <> INVALID_HANDLE_VALUE) then
  begin
    vte_terminal_set_font(fTerminalHanlde, PGtkWidget(Handle).style.font_desc);
  end;
  {$pop}
  {$endif}
end;

{$ifdef hasgtk2term}
initialization
  RegisterTerminal;
{$endif}
end.

