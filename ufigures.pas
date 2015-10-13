unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  //Classes, SysUtils, ExtCtrls;
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, Buttons;

type
  TFigure = Class(TObject)
    private
      FFigures: array of TFigure; static;
    public
      class procedure addFigure(figure: TFigure); static;
      class function getLastFigure(): TFigure; static;
      procedure Draw(PaintBox: TPaintBox); virtual; abstract;
  end;

  TPen = Class(TFigure)
    private
      FPoints: array of TPoint;
    public
      procedure addPoint(point: TPoint);
      procedure Draw(PaintBox: TPaintBox); override;
  end;

  TLine = Class(TFigure)
    public
      startP, endP: TPoint;
      procedure Draw(PaintBox: TPaintBox); override;
  end;

  TPolyline = Class(TFigure)
    private
      FLines: array of TLine;
    public
      procedure Draw(PaintBox: TPaintBox); override;
      procedure addLine();
      function getLastLine(): TLine;
  end;

  TRectangle = Class(TFigure)
    public
      startP, endP: TPoint;
      procedure Draw(PaintBox: TPaintBox); override;
  end;

  TRoundRectangle = Class(TFigure)
    public
      startP, endP: TPoint;
      procedure Draw(PaintBox: TPaintBox); override;
  end;

  TEllipse = Class(TFigure)
    public
      startP, endP: TPoint;
      procedure Draw(PaintBox: TPaintBox); override;
  end;

implementation

class procedure TFigure.addFigure(figure: TFigure);
begin
  SetLength(TFigure.FFigures, Length(TFigure.FFigures) + 1);
  TFigure.FFigures[High(TFigure.FFigures)]:= figure;
end;

class function TFigure.getLastFigure(): TFigure;
begin
  result:= FFigures[High(FFigures)];
end;

procedure TPen.addPoint(point: TPoint);
begin
  SetLength(FPoints, Length(FPoints) + 1);
  FPoints[High(FPoints)]:= point;
end;

procedure TPen.Draw(PaintBox: TPaintBox);
var
  point: TPoint;
begin
  PaintBox.Canvas.MoveTo(FPoints[0]);
  for point in FPoints do
      PaintBox.Canvas.LineTo(point);
end;

procedure TLine.Draw(PaintBox: TPaintBox);
begin
  with PaintBox.Canvas do begin
    MoveTo(startP);
    LineTo(endP);
  end;
end;

procedure TPolyline.Draw(PaintBox: TPaintBox);
var
  linee: TLine;
begin
  for linee in FLines do
    with PaintBox.Canvas do begin
      MoveTo(linee.startP);
      LineTo(linee.endP);
    end;
end;

procedure TPolyline.addLine();
begin
  SetLength(FLines, Length(FLines) + 1);
  FLines[High(FLines)]:= TLine.Create;
end;

function TPolyline.getLastLine(): TLine;
begin
  Result:= FLines[High(FLines)];
end;

procedure TRectangle.Draw(PaintBox: TPaintBox);
begin
  PaintBox.Canvas.Brush.Style:= bsClear;
  with PaintBox.Canvas do
    Rectangle(startP.x, startP.y, endP.x, endP.y);
end;

procedure TRoundRectangle.Draw(PaintBox: TPaintBox);
begin
  PaintBox.Canvas.Brush.Style:= bsClear;
  with PaintBox.Canvas do
    RoundRect(startP.x, startP.y, endP.x, endP.y, 10, 10);
end;

end.

