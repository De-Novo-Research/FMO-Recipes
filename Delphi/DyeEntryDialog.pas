unit DyeEntryDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.ComboEdit, FMX.Controls.Presentation, FMX.ListBox,
  FMX.EditBox, FMX.NumberBox,
  FMORecipe.Types;

type
  TdlgDyeEntry = class(TForm)
    lblColor: TLabel;
    cboColor: TComboEdit;
    lblAmount: TLabel;
    nbxAmount: TNumberBox;
    lblUnits: TLabel;
    lblLocation: TLabel;
    cboLocation: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FAllColorItems: TStringList;
    function ValidateInputs(out aErrorMsg: string): Boolean;
  public
    function GetColor: string;
    function GetAmount: Double;
    function GetLocation: TDyeLocation;
    procedure SetColor(const aValue: string);
    procedure SetAmount(aValue: Double);
    procedure SetLocation(aValue: TDyeLocation);
    procedure SetColorItems(aItems: TStrings);
  end;

var
  dlgDyeEntry: TdlgDyeEntry;

implementation

{$R *.fmx}

procedure TdlgDyeEntry.FormCreate(Sender: TObject);
begin
  FAllColorItems := TStringList.Create;

  cboLocation.Items.Clear;
  cboLocation.Items.Add(LOCATION_MAKE_FMO);
  cboLocation.Items.Add(LOCATION_COMMON_BACKBONE);
  cboLocation.ItemIndex := 0;
end;

procedure TdlgDyeEntry.FormDestroy(Sender: TObject);
begin
  FAllColorItems.Free;
end;

procedure TdlgDyeEntry.FormShow(Sender: TObject);
begin
  cboColor.SetFocus;
end;

function TdlgDyeEntry.ValidateInputs(out aErrorMsg: string): Boolean;
begin
  Result := False;
  aErrorMsg := '';

  if Trim(cboColor.Text) = '' then
  begin
    aErrorMsg := 'Please enter or select a color.';
    Exit;
  end;

  if nbxAmount.Value <= 0 then
  begin
    aErrorMsg := 'Amount must be greater than 0.';
    Exit;
  end;

  Result := True;
end;

procedure TdlgDyeEntry.btnOKClick(Sender: TObject);
var
  errorMsg: string;
begin
  if ValidateInputs(errorMsg) then
    ModalResult := mrOk
  else
    ShowMessage(errorMsg);
end;

procedure TdlgDyeEntry.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TdlgDyeEntry.GetColor: string;
begin
  Result := Trim(cboColor.Text);
end;

function TdlgDyeEntry.GetAmount: Double;
begin
  Result := nbxAmount.Value;
end;

function TdlgDyeEntry.GetLocation: TDyeLocation;
begin
  if cboLocation.ItemIndex = 0 then
    Result := dlFMO
  else
    Result := dlCommon;
end;

procedure TdlgDyeEntry.SetColor(const aValue: string);
begin
  cboColor.Text := aValue;
end;

procedure TdlgDyeEntry.SetAmount(aValue: Double);
begin
  nbxAmount.Value := aValue;
end;

procedure TdlgDyeEntry.SetLocation(aValue: TDyeLocation);
begin
  if aValue = dlFMO then
    cboLocation.ItemIndex := 0
  else
    cboLocation.ItemIndex := 1;
end;

procedure TdlgDyeEntry.SetColorItems(aItems: TStrings);
begin
  FAllColorItems.Assign(aItems);
  cboColor.Items.Assign(aItems);
end;

end.
