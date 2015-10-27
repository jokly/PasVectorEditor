unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, Buttons, UFigures, UCoordinateSystem;

type

  TTool = Class(TObject)
    protected
      FPenColor: TColor; static;
      FPenWidth: Integer; static;
    public
      Tools: array of TTool; static;
      ButtonOnForm: TBitBtn;
      ImageOfButton: TBitmap;
      constructor Create(PathToFile: String); overload;
      class procedure AddTool(Tool: TTool); static;
      class procedure SetPenColor(Color: TColor); static;
      class procedure SetPenWidth(Width: Integer); static;
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer); virtual; abstract;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X,
        Y: Integer); virtual; abstract;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer); virtual; abstract;
  end;

  TTPen = Class(TTool)
    public
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer); override;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X,
        Y: Integer); override;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer); override;
  end;

  TTLine = Class(TTool)
    public
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer); override;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X,
        Y: Integer); override;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer); override;
  end;

  TTPolyline = Class(TTool)
    public
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer); override;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X,
        Y: Integer); override;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer); override;
  end;

  TTRectangle = Class(TTool)
    public
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer); override;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X,
        Y: Integer); override;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer); override;
  end;

  TTRoundRectangle = Class(TTool)
    public
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer); override;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X,
        Y: Integer); override;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer); override;
  end;

  TTEllipse = Class(TTool)
    public
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer); override;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X,
        Y: Integer); override;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer); override;
  end;

  const
    SpaceBetweenButtons = 7;
    SizeOfButton = 35;

  var
    IsMouseDown: Boolean;

implementation

var
  IsMouseWasDown: Boolean;
  ButtonWasDown: TMouseButton;

constructor TTool.Create(PathToFile: String);
begin
  ImageOfButton:= TBitmap.Create;
  ImageOfButton.Height:= SizeOfButton;
  ImageOfButton.Width:= SizeOfButton;
  ImageOfButton.LoadFromFile(PathToFile);
end;

class procedure TTool.AddTool(Tool: TTool);
begin
  SetLength(Tools, Length(Tools) + 1);
  Tools[High(Tools)]:= Tool;
end;

class procedure TTool.SetPenColor(Color: TColor);
begin
  FPenColor:= Color;
end;

class procedure TTool.SetPenWidth(Width: Integer);
begin
  FPenWidth:= Width;
end;

procedure TTPen.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then begin
    ButtonWasDown:= mbRight;
    Exit;
  end
  else ButtonWasDown:= mbLeft;
  TFigure.AddFigure(TPen.Create(FPenColor, FPenWidth));
end;

procedure TTPen.OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ButtonWasDown = mbRight then Exit;
  (TFigure.GetLastFigure() as TPen).AddPoint(Point(X, Y));
end;

procedure TTPen.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TTLine.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then begin
    ButtonWasDown:= mbRight;
    Exit;
  end
  else ButtonWasDown:= mbLeft;
  TFigure.AddFigure(TLine.Create(FPenColor, FPenWidth));
  (TFigure.GetLastFigure() as TLine).StartP:= TWordPoint.WordPoint(X, Y);
end;

procedure TTLine.OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ButtonWasDown = mbRight then Exit;
  (TFigure.GetLastFigure() as TLine).EndP:= TWordPoint.WordPoint(X, Y);
end;

procedure TTLine.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ButtonWasDown = mbRight then Exit;
  (TFigure.GetLastFigure() as TLine).EndP:= TWordPoint.WordPoint(X, Y);
end;

procedure TTPolyline.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then begin
    (TFigure.GetLastFigure() as TPolyline).GetLastLine().EndP:= TWordPoint.WordPoint(X, Y);
    IsMouseWasDown:= False;
    Exit;
  end;
  if IsMouseWasDown then
    (TFigure.GetLastFigure() as TPolyline).GetLastLine().EndP:= TWordPoint.WordPoint(X, Y)
  else
     IsMouseWasDown:= True;
  TFigure.AddFigure(TPolyline.Create(FPenColor, FPenWidth));
  (TFigure.GetLastFigure() as TPolyline).AddLine;
  (TFigure.GetLastFigure() as TPolyline).GetLastLine().StartP:= TWordPoint.WordPoint(X, Y);
end;

procedure TTPolyline.OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  (TFigure.GetLastFigure() as TPolyline).GetLastLine().EndP:= TWordPoint.WordPoint(X, Y);
end;

procedure TTPolyline.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if IsMouseWasDown then IsMouseDown:= True;
end;

procedure TTRectangle.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then begin
    ButtonWasDown:= mbRight;
    Exit;
  end
  else ButtonWasDown:= mbLeft;
  TFigure.AddFigure(TRectangle.Create(FPenColor, FPenWidth));
  (TFigure.GetLastFigure() as TRectangle).StartP:= TWordPoint.WordPoint(X, Y);
end;

procedure TTRectangle.OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ButtonWasDown = mbRight then Exit;
  (TFigure.GetLastFigure() as TRectangle).EndP:= TWordPoint.WordPoint(X, Y);
end;

procedure TTRectangle.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ButtonWasDown = mbRight then Exit;
  (TFigure.GetLastFigure() as TRectangle).EndP:= TWordPoint.WordPoint(X, Y);
end;

procedure TTRoundRectangle.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then begin
    ButtonWasDown:= mbRight;
    Exit;
  end
  else ButtonWasDown:= mbLeft;
  TFigure.AddFigure(TRoundRectangle.Create(FPenColor, FPenWidth));
  (TFigure.GetLastFigure() as TRoundRectangle).StartP:= TWordPoint.WordPoint(X, Y);
end;

procedure TTRoundRectangle.OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ButtonWasDown = mbRight then Exit;
  (TFigure.GetLastFigure() as TRoundRectangle).EndP:= TWordPoint.WordPoint(X, Y);
end;

procedure TTRoundRectangle.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ButtonWasDown = mbRight then Exit;
  (TFigure.GetLastFigure() as TRoundRectangle).EndP:= TWordPoint.WordPoint(X, Y);
end;

procedure TTEllipse.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then begin
    ButtonWasDown:= mbRight;
    Exit;
  end
  else ButtonWasDown:= mbLeft;
  TFigure.AddFigure(TEllipse.Create(FPenColor, FPenWidth));
  (TFigure.GetLastFigure() as TEllipse).StartP:= TWordPoint.WordPoint(X, Y);
end;

procedure TTEllipse.OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ButtonWasDown = mbRight then Exit;
  (TFigure.GetLastFigure() as TEllipse).EndP:= TWordPoint.WordPoint(X, Y);
end;

procedure TTEllipse.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ButtonWasDown = mbRight then Exit;
  (TFigure.GetLastFigure() as TEllipse).EndP:= TWordPoint.WordPoint(X, Y);
end;

initialization
TTool.AddTool(TTPen.Create('img\pen.bmp'));
TTool.AddTool(TTLine.Create('img\line.bmp'));
TTool.AddTool(TTPolyline.Create('img\polyline.bmp'));
TTool.AddTool(TTRectangle.Create('img\rectangle.bmp'));
TTool.AddTool(TTRoundRectangle.Create('img\roundRect.bmp'));
TTool.AddTool(TTEllipse.Create('img\ellipse.bmp'));

end.

