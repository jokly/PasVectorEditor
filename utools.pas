unit UTools;

{$mode objfpc}{$H+}

interface

uses
  //Classes, SysUtils, UFigures, Dialogs;
Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, Buttons, UFigures;

type

  TTool = Class(TObject)
    public
      tools: array of TTool;
      procedure onMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer); virtual; abstract;
      procedure onMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer); virtual; abstract;
      procedure onMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer); virtual; abstract;
  end;

  TTPen = Class(TTool)
    public
      procedure onMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer); override;
      procedure onMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer); override;
      procedure onMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer); override;
  end;

  TTLine = Class(TTool)
    public
      procedure onMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer); override;
      procedure onMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer); override;
      procedure onMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer); override;
  end;

var
  point: TPoint;

implementation

procedure TTPen.onMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TFigure.addFigure(TPen.Create);
end;

procedure TTPen.onMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;
  point.x:= X;
  point.y:= Y;
  (TFigure.getLastFigure() as TPen).addPoint(point);
  (Sender as TPaintBox).Invalidate;
end;

procedure TTPen.onMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TTLine.onMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TFigure.addFigure(TLine.Create);
  point.x:= X;
  point.y:= Y;
  (TFigure.getLastFigure() as TLine).startP:= point;
end;

procedure TTLine.onMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;
  point.x:= X;
  point.y:= Y;
  (TFigure.getLastFigure() as TLine).endP:= point;
  (Sender as TPaintBox).Invalidate;
end;

procedure TTLine.onMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  point.x:= X;
  point.y:= Y;
  (TFigure.getLastFigure() as TLine).endP:= point;
  (Sender as TPaintBox).Invalidate;
end;

end.

