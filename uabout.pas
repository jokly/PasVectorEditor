unit UAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TaboutForm }

  TaboutForm = class(TForm)
    Memo: TMemo;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  aboutForm: TaboutForm;

implementation

{$R *.lfm}

{ TaboutForm }


end.

