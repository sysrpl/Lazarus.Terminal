unit TerminalCtrls;

{$mode delphi}

interface

uses
  Classes, SysUtils, Controls, Graphics;

{ TTerminal }

type
  TTerminal = class(TCustomControl)
  private
    FOnTerminate: TNotifyEvent;
    FInfo: Pointer;
  protected
    procedure Paint; override;
    procedure DoTerminate; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Restart;
  published
    property Align;
    property Anchors;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
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
  end;

function TerminalAvaiable: Boolean;

implementation

{$ifdef lclgtk2}
uses
  LCLType, WSControls, WSLCLClasses, GLib2, Gtk2, Gtk2Def, Gtk2Proc,
  Gtk2WSControls, Gtk2Term;

{ TGtk2WSTerminal }

type
  TGtk2WSTerminal = class(TWSCustomControl)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

procedure TerminalExit(Widget: PGtkWidget); cdecl;
var
  Info: PWidgetInfo;
begin
  Info := PWidgetInfo(g_object_get_data(PGObject(Widget), 'widgetinfo'));
  TTerminal(Info.LCLObject).DoTerminate;
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
  Args: array[0..1] of PChar = ('/bin/bash', nil);
  Allocation: TGTKAllocation;
begin
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
    Info.ClientWidget := vte_terminal_new;
    vte_terminal_fork_command_full(VTE_TERMINAL(Info.ClientWidget), VTE_PTY_DEFAULT,
      nil, @Args[0], nil, G_SPAWN_SEARCH_PATH, nil, nil, nil, nil);
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

procedure TTerminal.DoTerminate;
begin
  if Assigned(FOnTerminate) then
    FOnTerminate(Self);
end;

procedure TTerminal.Restart;
var
  Info: PWidgetInfo;
  Args: array[0..1] of PChar = ('/bin/bash', nil);
begin
  if not HandleAllocated then
    Exit;
  Info := PWidgetInfo(FInfo);
  gtk_widget_destroy(Info.ClientWidget);
  Info.ClientWidget := vte_terminal_new;
  vte_terminal_fork_command_full(VTE_TERMINAL(Info.ClientWidget), VTE_PTY_DEFAULT,
    nil, @Args[0], nil, G_SPAWN_SEARCH_PATH, nil, nil, nil, nil);
  gtk_container_add(GTK_CONTAINER(Info.CoreWidget), Info.ClientWidget);
  g_object_set_data(PGObject(Info.ClientWidget), 'widgetinfo', Info);
  gtk_widget_show_all(Info.CoreWidget);
  g_signal_connect(Info.ClientWidget, 'child-exited', G_CALLBACK(@TerminalExit), nil);
end;

function TerminalAvaiable: Boolean;
begin
  Result := Gtk2TermLoad;
end;

function RegisterTerminal: Boolean;
begin
  Result := TerminalAvaiable;
  if Result then
    RegisterWSComponent(TTerminal, TGtk2WSTerminal);
end;
{$else}
function TerminalAvaiable: Boolean;
begin
  Result := False;
end;

procedure TTerminal.DoTerminate;
begin
end;

procedure TTerminal.Restart;
begin
end;
{$endif}

constructor TTerminal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 300;
  Height := 200;
end;

procedure TTerminal.Paint;
const
  S = 'linux@user:~$ bash terminal';
var
  W, H: Integer;
begin
  Canvas.Pen.Style := psDash;
  Canvas.Pen.Color := clWhite;
  Canvas.Brush.Color := clBlack;
  Canvas.Font.Color := clWhite;
  Canvas.FillRect(ClientRect);
  Canvas.Rectangle(ClientRect);
  W := 0; H := 0;
  Canvas.GetTextSize(S, W, H);
  Canvas.TextOut((Width -  W) div 2, (Height -  H) div 2, S);
end;

{$ifdef lclgtk2}
initialization
  RegisterTerminal;
{$endif}
end.

