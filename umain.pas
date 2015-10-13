unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, Buttons, UAbout, UTools, UFigures;

type

  { TMainForm }

  TMainForm = class(TForm)
    Pen: TBitBtn;
    Line: TBitBtn;
    MainMenu: TMainMenu;
    MFile: TMenuItem;
    MAbout: TMenuItem;
    MExit: TMenuItem;
    PaintBox: TPaintBox;
    Polyline: TBitBtn;
    ToolsPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure MAboutClick(Sender: TObject);
    procedure MExitClick(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure ToolClick(Sender: TObject);
  end;

  states = (Pen, Line);

var
  mainForm: TMainForm;
  toolPen, toolLine: TTool;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  toolPen:= TTPen.Create;
  toolLine:= TTLine.Create;
end;

procedure TMainForm.MAboutClick(Sender: TObject);
begin
  aboutForm.Show;
end;

procedure TMainForm.MExitClick(Sender: TObject);
begin
  mainForm.Close;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  figure: TFigure;
begin
  for figure in TFigure.figures do
      figure.Draw(PaintBox);
end;

procedure TMainForm.ToolClick(Sender: TObject);
begin
  case TButton(Sender).Name of
       'Pen': begin
         mainForm.PaintBox.OnMouseDown:= @toolPen.onMouseDown;
         mainForm.PaintBox.OnMouseMove:= @toolPen.onMouseMove;
         mainForm.PaintBox.OnMouseUp:= @toolPen.onMouseUp;
       end;
       'Line': begin
         mainForm.PaintBox.OnMouseDown:= @toolLine.onMouseDown;
         mainForm.PaintBox.OnMouseMove:= @toolLine.onMouseMove;
         mainForm.PaintBox.OnMouseUp:= @toolLine.onMouseUp;
       end;
  end;
end;

end.

