unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, about;

type

  { TmainForm }

  TmainForm = class(TForm)
    MainMenu: TMainMenu;
    MFile: TMenuItem;
    MAbout: TMenuItem;
    MExit: TMenuItem;
    PaintBox: TPaintBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MAboutClick(Sender: TObject);
    procedure MExitClick(Sender: TObject);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fout: text;
  mainForm: TmainForm;
  arrayOfPoints: array of TPoint;
  point: TPoint;
implementation

{$R *.lfm}

{ TmainForm }

procedure TmainForm.FormCreate(Sender: TObject);
begin
  system.Assign(fout, 'log.txt');
  rewrite(fout);
  SetLength(arrayOfPoints, 0)
end;

procedure TmainForm.MAboutClick(Sender: TObject);
begin
  aboutForm.Show;
end;

procedure TmainForm.MExitClick(Sender: TObject);
begin
  mainForm.Close;
end;

procedure TmainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  system.Close(fout);
end;

procedure TmainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;
  point.x:= X; point.y:= Y;
  SetLength(arrayOfPoints, Length(arrayOfPoints) + 1);
  arrayOfPoints[Length(arrayOfPoints) - 1]:= point;
  Invalidate;
end;

procedure TmainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  point.x:= -1; point.y:= -1;
  SetLength(arrayOfPoints, Length(arrayOfPoints) + 1);
  arrayOfPoints[Length(arrayOfPoints) - 1]:= point;
end;

procedure TmainForm.PaintBoxPaint(Sender: TObject);
var
  i:integer;
begin
  if (Length(arrayOfPoints) < 1) then Exit;
  PaintBox.Canvas.MoveTo(arrayOfPoints[0]);
  for i:=1 to Length(arrayOfPoints) - 1 do begin
    with PaintBox.Canvas do begin
      if(arrayOfPoints[i].x <> -1) then
        LineTo(arrayOfPoints[i])
      else MoveTo(arrayOfPoints[i+1]);
    end;
  end;
end;

end.

