unit UFigures;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Graphics, windows, UCoordinateSystem;

type

  TFigure = Class(TObject)
    private
      FPenColor: TColor;
      FPenWidth: Integer;
      FPenStyle: TPenStyle;
    public
      procedure Draw(Canvas: TCanvas); virtual; abstract;
      procedure SetPenColor(Color: TColor);
      class procedure AddFigure(Figure: TFigure);
      class function GetLastFigure(): TFigure;
      class procedure DeleteLastFigure();
    published
      property PenWidth: Integer read FPenWidth write FPenWidth default 1;
      property PenStyle: TPenStyle read FPenStyle write FPenStyle default psSolid;
  end;

  TPen = Class(TFigure)
    private
      FPoints: array of TWorldPoint;
    public
      procedure AddPoint(Point: TWorldPoint);
      procedure Draw(Canvas: TCanvas); override;
  end;

  TLine = Class(TFigure)
    public
      StartP, EndP: TWorldPoint;
      procedure Draw(Canvas: TCanvas); override;
  end;

  TPolyline = Class(TFigure)
    private
      FLines: array of TLine;
    public
      procedure Draw(Canvas: TCanvas); override;
      procedure AddLine();
      function GetLastLine(): TLine;
  end;

  TFillFigure = Class(TFigure)
    private
      FBrushColor: TColor;
      FBrushStyle: TBrushStyle;
    public
      StartP, EndP: TWorldPoint;
      procedure SetBrushColor(Color: TColor);
    published
      property BrushStyle: TBrushStyle read FBrushStyle write FBrushStyle default bsClear;
  end;

  TRectangle = Class(TFillFigure)
    public
      procedure Draw(Canvas: TCanvas); override;
  end;

  TRoundRectangle = Class(TFillFigure)
    private
      FRoundingX: Integer;
      FRoundingY: Integer;
    public
      procedure Draw(Canvas: TCanvas); override;
    published
      property RoundingX: Integer
        read FRoundingX
        write FRoundingX
        default 20;
      property RoundingY: Integer
        read FRoundingY
        write FRoundingY
        default 20;
  end;

  TEllipse = Class(TFillFigure)
    public
      procedure Draw(Canvas: TCanvas); override;
  end;

  var
    FFigures: array of TFigure;

implementation

class procedure TFigure.AddFigure(Figure: TFigure);
begin
  if Figure = Nil then Exit;
  SetLength(FFigures, Length(FFigures) + 1);
  FFigures[High(FFigures)]:= Figure;
end;

class function TFigure.GetLastFigure(): TFigure;
begin
  if Length(FFigures) > 0 then
    Result:= FFigures[High(FFigures)]
  else
    Result:= Nil;
end;

class procedure TFigure.DeleteLastFigure();
begin
  if Length(FFigures) > 0 then
    SetLength(FFigures, Length(FFigures) - 1);
end;

procedure TFigure.SetPenColor(Color: TColor);
begin
  FPenColor:= Color;
end;

procedure TPen.AddPoint(Point: TWorldPoint);
begin
  SetLength(FPoints, Length(FPoints) + 1);
  FPoints[High(FPoints)]:= Point;
end;

procedure TPen.Draw(Canvas: TCanvas);
var
  ScPoints: array of TPoint;
  i: Integer;
begin
  with Canvas do begin
    Pen.Color:= FPenColor;
    Pen.Width:= PenWidth;
    Pen.Style:= PenStyle;
    SetLength(ScPoints, Length(FPoints));
    for i:= 0 to High(FPoints) do
      ScPoints[i]:= ToScreenPoint(FPoints[i]);
    Polyline(ScPoints);
  end;
end;

procedure TLine.Draw(Canvas: TCanvas);
var
  SPoint, EPoint: TPoint;
begin
  SPoint:= ToScreenPoint(StartP);
  EPoint:= ToScreenPoint(EndP);
  if (SPoint.X = EPoint.X) and (SPoint.Y = EPoint.Y) then Exit;
  with Canvas do begin
    Pen.Color:= FPenColor;
    Pen.Width:= PenWidth;
    Pen.Style:= PenStyle;
    MoveTo(SPoint);
    LineTo(EPoint);
  end;
end;

procedure TPolyline.Draw(Canvas: TCanvas);
var
  _line: TLine;
begin
  Canvas.Pen.Color:= FPenColor;
  Canvas.Pen.Width:= PenWidth;
  Canvas.Pen.Style:= PenStyle;
  for _line in FLines do begin
    with Canvas do begin
      MoveTo(ToScreenPoint(_line.StartP));
      LineTo(ToScreenPoint(_line.EndP));
    end;
  end;
end;

procedure TPolyline.AddLine();
begin
  SetLength(FLines, Length(FLines) + 1);
  FLines[High(FLines)]:= TLine.Create();
end;

function TPolyline.GetLastLine(): TLine;
begin
  if FLines = Nil then Exit(nil);
  if (Length(FLines) > 0) then
    Result:= FLines[High(FLines)]
  else
    Result:= Nil;
end;

procedure TFillFigure.SetBrushColor(Color: TColor);
begin
  FBrushColor:= Color;
end;

procedure TRectangle.Draw(Canvas: TCanvas);
begin
  with Canvas do begin
    Pen.Color:= FPenColor;
    Pen.Width:= PenWidth;
    Pen.Style:= PenStyle;
    Brush.Color:= FBrushColor;
    Brush.Style:= BrushStyle;
    Rectangle(ToScreenPoint(StartP).x, ToScreenPoint(StartP).y,
              ToScreenPoint(EndP).x, ToScreenPoint(EndP).y);
  end;
end;

procedure TRoundRectangle.Draw(Canvas: TCanvas);
begin
  with Canvas do begin
    Pen.Color:= FPenColor;
    Pen.Width:= PenWidth;
    Pen.Style:= PenStyle;
    Brush.Color:= FBrushColor;
    Brush.Style:= BrushStyle;
    RoundRect(ToScreenPoint(StartP).x, ToScreenPoint(StartP).y,
              ToScreenPoint(EndP).x, ToScreenPoint(EndP).y,
              RoundingX, RoundingY);
  end;
end;

procedure TEllipse.Draw(Canvas: TCanvas);
begin
  with Canvas do begin
    Pen.Color:= FPenColor;
    Pen.Width:= PenWidth;
    Pen.Style:= PenStyle;
    Brush.Color:= FBrushColor;
    Brush.Style:= BrushStyle;
    Ellipse(ToScreenPoint(StartP).x, ToScreenPoint(StartP).y,
            ToScreenPoint(EndP).x, ToScreenPoint(EndP).y);
  end;
end;

end.

