unit unitabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormabout }

  TFormabout = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    labelCompileDate: TLabel;
    labelCompileVer: TLabel;
    LabelCompileTime: TLabel;
    labelCompileOs: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Formabout: TFormabout;

implementation

{$R *.lfm}

{ TFormabout }

procedure TFormabout.Button1Click(Sender: TObject);
begin
  formabout.Close;
end;

procedure TFormabout.FormCreate(Sender: TObject);
begin
 labelCompileTime.Caption:= ( 'This program was compiled at ' + {$I %TIME%} );
  labelCompileDate.Caption:= ( 'On ' + {$I %DATE%} );
  labelCompileVer.Caption:= ( 'FPC ' + {$I %FPCVERSION%} );
  labelCompileOs.Caption:= ( 'FPC ' + {$I %FPCTARGET%} );
end;

end.

