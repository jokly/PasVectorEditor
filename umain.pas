unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, Buttons, UAbout, UTools, UFigures;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    MFile: TMenuItem;
    MAbout: TMenuItem;
    MExit: TMenuItem;
    PaintBox: TPaintBox;
    ToolsPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure MAboutClick(Sender: TObject);
    procedure MExitClick(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure ToolClick(Sender: TObject);
  end;

  states = (Pen, Line);

const
  spaceBetweenButtons = 10;
  sizeOfButton = 50;

var
  mainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  i, addLeft, addTop: integer;
begin
  addLeft:= 0;
  addTop:= 0;
  ToolsPanel.Width:= 3 * spaceBetweenButtons + 2 * sizeOfButton;
  for i:=0 to High(TTool.Tools) do begin
    TTool.Tools[i].ButtonOnForm:= TBitBtn.Create(Self);
    with TTool.Tools[i].ButtonOnForm do begin
      Name:= TTool.Tools[i].ToString + IntToStr(i);
      Caption:= '';
      Parent:= Self;
      Width:= sizeOfButton;
      Height:= sizeOfButton;
      Glyph:= TTool.Tools[i].ImageOfButton;
      addTop:= (i div 2) * (spaceBetweenButtons + sizeOfButton);
      Left:= spaceBetweenButtons + addLeft;
      Top:= spaceBetweenButtons + addTop;
      if addLeft = 0 then
         addLeft:= spaceBetweenButtons + sizeOfButton
      else
         addLeft:= 0;
      OnClick:= @mainForm.ToolClick;
      Tag:= i;
    end;
  end;
  TTool.Tools[0].ButtonOnForm.Click;
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
  for figure in TFigure.FFigures do
      figure.Draw(PaintBox);
end;

procedure TMainForm.ToolClick(Sender: TObject);
begin
  mainForm.PaintBox.OnMouseDown:= @TTool.Tools[(Sender as TBitBtn).Tag].onMouseDown;
  mainForm.PaintBox.OnMouseMove:= @TTool.Tools[(Sender as TBitBtn).Tag].OnMouseMove;
  mainForm.PaintBox.OnMouseUp:= @TTool.Tools[(Sender as TBitBtn).Tag].OnMouseUp;
end;

end.

