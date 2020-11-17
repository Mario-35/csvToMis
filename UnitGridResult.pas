unit UnitGridResult;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DBGrids, DB, StdCtrls, Buttons, ExtCtrls;

type
  TFormResultGrid = class(TForm)
    PanelBas: TPanel;
    BitBtnClose: TBitBtn;
    DataSource1: TDataSource;
    DBGridResult: TDBGrid;
    procedure FormResize(Sender: TObject);
    procedure DBGridResultDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormResultGrid: TFormResultGrid;

implementation

{$R *.dfm}

procedure TFormResultGrid.FormResize(Sender: TObject);
  begin
    BitBtnClose.Left := PanelBas.Width -(BitBtnClose.Width + 10);
  end;

procedure TFormResultGrid.DBGridResultDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
  begin
    // if Odd(TDBGrid(Sender).DataSource.DataSet.RecNo)
    // Then TDBGrid(Sender).Canvas.Brush.Color := clSilver
    //  Else TDBGrid(Sender).Canvas.Brush.Color := clDkGray;
    TDBGrid(Sender).Canvas.FillRect(Rect);
    TDBGrid(Sender).Canvas.TextOut(Rect.Left+1, Rect.Top+2, StringReplace(Column.Field.Value, #13#10, ' ', [rfReplaceAll]));
  end;

end.
