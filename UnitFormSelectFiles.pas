unit UnitFormSelectFiles;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, Buttons, ExtCtrls;

type
  TFormSelectFiles = class(TForm)
    PanelBas: TPanel;
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    CheckListBoxLog: TCheckListBox;
    procedure FormResize(Sender: TObject);
    procedure BitBtnOkClick(Sender: TObject);
    procedure CheckListBoxLogClickCheck(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSelectFiles: TFormSelectFiles;

implementation

{$R *.dfm}

procedure TFormSelectFiles.FormResize(Sender: TObject);
begin
  begin
    BitBtnCancel.Left := PanelBas.Width -(BitBtnCancel.Width + 10);
    BitBtnOk.Left := BitBtnCancel.Left -(BitBtnOk.Width + 10);
  end;
end;

procedure TFormSelectFiles.BitBtnOkClick(Sender: TObject);
begin
  modalResult := mrOk;
end;

procedure TFormSelectFiles.CheckListBoxLogClickCheck(Sender: TObject);
  Var
    i : integer;

  Begin
    For i := 0 To CheckListBoxLog.Count -1
      Do If CheckListBoxLog.Checked[i]
        Then Begin
          BitBtnOk.Enabled := True;
          Exit;
        End;
     BitBtnOk.Enabled := False;

end;

end.
