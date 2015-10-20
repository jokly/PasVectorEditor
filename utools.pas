unit UTools;

{$mode objfpc}{$H+}

interface

uses
  //Classes, SysUtils, UFigures, Dialogs;
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, Buttons, UFigures;

type

  TTool = Class(TObject)
    protected
      FPenColor: TColor; static;
      FPenWidth: Integer; static;
    public
      Tools: array of TTool; static;
      ButtonOnForm: TBitBtn;
      ImageOfButton: TBitmap;
      constructor Create(pathToFile: String); overload;
      class procedure addTool(tool: TTool); static;
      class procedure setPenColor(color: TColor); static;
      class procedure setPenWidth(width: Integer); static;
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

implementation

var
  isMouseWasDown: Boolean;

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

class procedure TTool.setPenColor(color: TColor);
begin
  FPenColor:= color;
end;

class procedure TTool.setPenWidth(width: Integer);
begin
  FPenWidth:= width;
end;

procedure TTPen.onMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TFigure.addFigure(TPen.Create(FPenColor, FPenWidth));
end;

procedure TTPen.onMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not(ssLeft in Shift) and not(ssRight in Shift) then Exit;
  (TFigure.getLastFigure() as TPen).addPoint(Point(X, Y));
end;

procedure TTPen.onMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TTLine.onMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TFigure.addFigure(TLine.Create(FPenColor, FPenWidth));
  (TFigure.getLastFigure() as TLine).startP:= Point(X, Y);
end;

procedure TTLine.onMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not(ssLeft in Shift) and not(ssRight in Shift) then
    Exit;
  (TFigure.getLastFigure() as TLine).endP:= Point(X, Y);
end;

procedure TTLine.onMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  (TFigure.getLastFigure() as TLine).endP:= Point(X, Y);
end;

procedure TTPolyline.onMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then begin
    (TFigure.getLastFigure() as TPolyline).getLastLine().endP:= Point(X, Y);
    isMouseWasDown:= false;
    Exit;
  end;
  if isMouseWasDown then  begin
    (TFigure.getLastFigure() as TPolyline).getLastLine().endP:= Point(X, Y);
  end
  else
     isMouseWasDown:= true;
  TFigure.addFigure(TPolyline.Create(FPenColor, FPenWidth));
  (TFigure.getLastFigure() as TPolyline).addLine;
  (TFigure.getLastFigure() as TPolyline).getLastLine().startP:= Point(X, Y);
end;

procedure TTPolyline.onMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not(isMouseWasDown) and not(ssRight in Shift)  then
    Exit;
  (TFigure.getLastFigure() as TPolyline).getLastLine().endP:= Point(X, Y);
end;

procedure TTPolyline.onMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TTRectangle.onMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TFigure.addFigure(TRectangle.Create(FPenColor, FPenWidth));
  (TFigure.getLastFigure() as TRectangle).startP:= Point(X, Y);
end;

procedure TTRectangle.onMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not (ssLeft in Shift) and not(ssRight in Shift)  then
    Exit;
  (TFigure.getLastFigure() as TRectangle).endP:= Point(X, Y);
end;

procedure TTRectangle.onMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  (TFigure.getLastFigure() as TRectangle).endP:= Point(X, Y);
end;

procedure TTRoundRectangle.onMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TFigure.addFigure(TRoundRectangle.Create(FPenColor, FPenWidth));
  (TFigure.getLastFigure() as TRoundRectangle).startP:= Point(X, Y);
end;

procedure TTRoundRectangle.onMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not (ssLeft in Shift) and not(ssRight in Shift)  then
    Exit;
  (TFigure.getLastFigure() as TRoundRectangle).endP:= Point(X, Y);
end;

procedure TTRoundRectangle.onMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  (TFigure.getLastFigure() as TRoundRectangle).endP:= Point(X, Y);
end;

procedure TTEllipse.onMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TFigure.addFigure(TEllipse.Create(FPenColor, FPenWidth));
  (TFigure.getLastFigure() as TEllipse).startP:= Point(X, Y);
end;

procedure TTEllipse.onMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not (ssLeft in Shift) and not(ssRight in Shift)  then
    Exit;
  (TFigure.getLastFigure() as TEllipse).endP:= Point(X, Y);
end;

procedure TTEllipse.onMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  (TFigure.getLastFigure() as TEllipse).endP:= Point(X, Y);
end;

initialization
TTool.addTool(TTPen.Create('img\pen.bmp'));
TTool.addTool(TTLine.Create('img\line.bmp'));
TTool.addTool(TTPolyline.Create('img\polyline.bmp'));
TTool.addTool(TTRectangle.Create('img\rectangle.bmp'));
TTool.addTool(TTRoundRectangle.Create('img\roundRect.bmp'));
TTool.addTool(TTEllipse.Create('img\ellipse.bmp'));

end.

