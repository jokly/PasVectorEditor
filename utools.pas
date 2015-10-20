unit UTools;

{$mode objfpc}{$H+}

interface

uses
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

implementation

var
  IsMouseWasDown: Boolean;

constructor TTool.Create(PathToFile: String);
begin
  ImageOfButton:= TBitmap.Create;
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
  TFigure.AddFigure(TPen.Create(FPenColor, FPenWidth));
end;

procedure TTPen.OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not(ssLeft in Shift) and not(ssRight in Shift) then Exit;
  (TFigure.GetLastFigure() as TPen).AddPoint(Point(X, Y));
end;

procedure TTPen.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TTLine.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TFigure.AddFigure(TLine.Create(FPenColor, FPenWidth));
  (TFigure.GetLastFigure() as TLine).StartP:= Point(X, Y);
end;

procedure TTLine.OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not(ssLeft in Shift) and not(ssRight in Shift) then
    Exit;
  (TFigure.GetLastFigure() as TLine).EndP:= Point(X, Y);
end;

procedure TTLine.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  (TFigure.GetLastFigure() as TLine).EndP:= Point(X, Y);
end;

procedure TTPolyline.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then begin
    (TFigure.GetLastFigure() as TPolyline).GetLastLine().EndP:= Point(X, Y);
    IsMouseWasDown:= False;
    Exit;
  end;
  if IsMouseWasDown then  begin
    (TFigure.GetLastFigure() as TPolyline).GetLastLine().EndP:= Point(X, Y);
  end
  else
     IsMouseWasDown:= True;
  TFigure.AddFigure(TPolyline.Create(FPenColor, FPenWidth));
  (TFigure.GetLastFigure() as TPolyline).AddLine;
  (TFigure.GetLastFigure() as TPolyline).GetLastLine().StartP:= Point(X, Y);
end;

procedure TTPolyline.OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not(IsMouseWasDown) and not(ssRight in Shift)  then
    Exit;
  (TFigure.GetLastFigure() as TPolyline).GetLastLine().EndP:= Point(X, Y);
end;

procedure TTPolyline.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TTRectangle.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TFigure.AddFigure(TRectangle.Create(FPenColor, FPenWidth));
  (TFigure.GetLastFigure() as TRectangle).StartP:= Point(X, Y);
end;

procedure TTRectangle.OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not (ssLeft in Shift) and not(ssRight in Shift)  then
    Exit;
  (TFigure.GetLastFigure() as TRectangle).EndP:= Point(X, Y);
end;

procedure TTRectangle.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  (TFigure.GetLastFigure() as TRectangle).EndP:= Point(X, Y);
end;

procedure TTRoundRectangle.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TFigure.AddFigure(TRoundRectangle.Create(FPenColor, FPenWidth));
  (TFigure.GetLastFigure() as TRoundRectangle).StartP:= Point(X, Y);
end;

procedure TTRoundRectangle.OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not (ssLeft in Shift) and not(ssRight in Shift)  then
    Exit;
  (TFigure.GetLastFigure() as TRoundRectangle).EndP:= Point(X, Y);
end;

procedure TTRoundRectangle.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  (TFigure.GetLastFigure() as TRoundRectangle).EndP:= Point(X, Y);
end;

procedure TTEllipse.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TFigure.AddFigure(TEllipse.Create(FPenColor, FPenWidth));
  (TFigure.GetLastFigure() as TEllipse).StartP:= Point(X, Y);
end;

procedure TTEllipse.OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not (ssLeft in Shift) and not(ssRight in Shift)  then
    Exit;
  (TFigure.GetLastFigure() as TEllipse).EndP:= Point(X, Y);
end;

procedure TTEllipse.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  (TFigure.GetLastFigure() as TEllipse).EndP:= Point(X, Y);
end;

initialization
TTool.AddTool(TTPen.Create('img\pen.bmp'));
TTool.AddTool(TTLine.Create('img\line.bmp'));
TTool.AddTool(TTPolyline.Create('img\polyline.bmp'));
TTool.AddTool(TTRectangle.Create('img\rectangle.bmp'));
TTool.AddTool(TTRoundRectangle.Create('img\roundRect.bmp'));
TTool.AddTool(TTEllipse.Create('img\ellipse.bmp'));

end.

