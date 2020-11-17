unit UnitListSelect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TFormListSelect = class(TForm)
    PanelBas: TPanel;
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    ListBoxItems: TListBox;
    procedure ListBoxItemsDblClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormListSelect: TFormListSelect;

implementation

{$R *.dfm}

procedure TFormListSelect.ListBoxItemsDblClick(Sender: TObject);
  begin
    modalResult := mrOk;
  end;

procedure TFormListSelect.FormResize(Sender: TObject);
  begin
    BitBtnCancel.Left := PanelBas.Width -(BitBtnCancel.Width + 10);
    BitBtnOk.Left := BitBtnCancel.Left -(BitBtnOk.Width + 10);
  end;

end.
