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
      Tools: array of TTool; static;
      ButtonOnForm: TBitBtn;
      ImageOfButton: TBitmap;
      constructor Create(pathToFile: String); overload;
      class procedure addTool(tool: TTool); static;
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

  TTPolyline = Class(TTool)
    public
      isMouseWasDown: boolean;
      procedure onMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer); override;
      procedure onMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer); override;
      procedure onMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer); override;
  end;

  TTRectangle = Class(TTool)
    public
      procedure onMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer); override;
      procedure onMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer); override;
      procedure onMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer); override;
  end;

  TTRoundRectangle = Class(TTool)
    public
      procedure onMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer); override;
      procedure onMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer); override;
      procedure onMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer); override;
  end;

  TTEllipse = Class(TTool)
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

constructor TTool.Create(pathToFile: String);
begin
  ImageOfButton:= TBitmap.Create;
  ImageOfButton.LoadFromFile(pathToFile);
end;

class procedure TTool.addTool(tool: TTool);
begin
  SetLength(Tools, Length(Tools) + 1);
  Tools[High(Tools)]:= tool;
end;

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

procedure TTPolyline.onMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  point.x:= X;
  point.y:= Y;
  if Button = mbRight then begin
    (TFigure.getLastFigure() as TPolyline).getLastLine().endP:= point;
    isMouseWasDown:= false;
    Exit;
  end;
  if isMouseWasDown then  begin
    (TFigure.getLastFigure() as TPolyline).getLastLine().endP:= point;
  end
  else
     isMouseWasDown:= true;
  TFigure.addFigure(TPolyline.Create);
  (TFigure.getLastFigure() as TPolyline).addLine;
  (TFigure.getLastFigure() as TPolyline).getLastLine().startP:= point;
end;

procedure TTPolyline.onMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not(isMouseWasDown) then
    Exit;
  point.x:= X;
  point.y:= Y;
  (TFigure.getLastFigure() as TPolyline).getLastLine().endP:= point;
  (Sender as TPaintBox).Invalidate;
end;

procedure TTPolyline.onMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TTRectangle.onMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TFigure.addFigure(TRectangle.Create);
  point.x:= X;
  point.y:= Y;
  (TFigure.getLastFigure() as TRectangle).startP:= point;
end;

procedure TTRectangle.onMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;
  point.x:= X;
  point.y:= Y;
  (TFigure.getLastFigure() as TRectangle).endP:= point;
  (Sender as TPaintBox).Invalidate;
end;

procedure TTRectangle.onMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  point.x:= X;
  point.y:= Y;
  (TFigure.getLastFigure() as TRectangle).endP:= point;
end;

procedure TTRoundRectangle.onMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TFigure.addFigure(TRoundRectangle.Create);
  point.x:= X;
  point.y:= Y;
  (TFigure.getLastFigure() as TRoundRectangle).startP:= point;
end;

procedure TTRoundRectangle.onMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;
  point.x:= X;
  point.y:= Y;
  (TFigure.getLastFigure() as TRoundRectangle).endP:= point;
  (Sender as TPaintBox).Invalidate;
end;

procedure TTRoundRectangle.onMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  point.x:= X;
  point.y:= Y;
  (TFigure.getLastFigure() as TRoundRectangle).endP:= point;
end;

procedure TTEllipse.onMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TFigure.addFigure(TEllipse.Create);
  point.x:= X;
  point.y:= Y;
  (TFigure.getLastFigure() as TEllipse).startP:= point;
end;

procedure TTEllipse.onMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;
  point.x:= X;
  point.y:= Y;
  (TFigure.getLastFigure() as TEllipse).endP:= point;
  (Sender as TPaintBox).Invalidate;
end;

procedure TTEllipse.onMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  point.x:= X;
  point.y:= Y;
  (TFigure.getLastFigure() as TEllipse).endP:= point;
end;

initialization
TTool.addTool(TTPen.Create('C:\Users\slast\Documents\FreePascal\pasVectorEditor\img\pen.bmp'));
TTool.addTool(TTLine.Create('C:\Users\slast\Documents\FreePascal\pasVectorEditor\img\line.bmp'));
TTool.addTool(TTPolyline.Create('C:\Users\slast\Documents\FreePascal\pasVectorEditor\img\polyline.bmp'));
TTool.addTool(TTRectangle.Create('C:\Users\slast\Documents\FreePascal\pasVectorEditor\img\rectangle.bmp'));
TTool.addTool(TTRoundRectangle.Create('C:\Users\slast\Documents\FreePascal\pasVectorEditor\img\roundRect.bmp'));
TTool.addTool(TTEllipse.Create('C:\Users\slast\Documents\FreePascal\pasVectorEditor\img\ellipse.bmp'));

end.

