unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections, System.IOUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Grid.Style, FMX.Grid, FMX.ScrollBox, FMX.Menus, FMX.Memo,
  System.Rtti,
  FMORecipe.Types, FMX.Memo.Types;

type
  TfrmMain = class(TForm)
    btnAdd: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    btnClear: TButton;
    grdDyes: TStringGrid;
    colColor: TStringColumn;
    colAmount: TStringColumn;
    colLocation: TStringColumn;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    RecipeSaveDialog: TSaveDialog;
    MainMenu: TMainMenu;
    mnuFile: TMenuItem;
    mnuFileSave: TMenuItem;
    mnuFileLoad: TMenuItem;
    mnuFileSep1: TMenuItem;
    mnuFileSaveRecipe: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuBuild: TMenuItem;
    mnuBuildRecipe: TMenuItem;
    lblRecipe: TLabel;
    mmoRecipe: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnGenerateRecipeClick(Sender: TObject);
    procedure grdDyesCellDblClick(const Column: TColumn; const Row: Integer);
    procedure grdDyesSelectCell(Sender: TObject; const ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure mnuFileSaveClick(Sender: TObject);
    procedure mnuFileLoadClick(Sender: TObject);
    procedure mnuFileExitClick(Sender: TObject);
    procedure mnuFileSaveRecipeClick(Sender: TObject);
    procedure mnuBuildRecipeClick(Sender: TObject);
  private
    FDyeEntries: TDyeEntryList;
    FDyeNames: TStringList;
    procedure LoadDyeNames;
    procedure SaveDyeNames;
    procedure UpdateDyeNamesFromEntries;
    procedure RefreshGrid;
  public
  end;

var
  frmMain: TfrmMain;

implementation

uses
  DyeEntryDialog, FMORecipe.Sets;

{$R *.fmx}

const
  DYE_NAMES_FILE = 'dye_names.txt';

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FDyeEntries := TDyeEntryList.Create;
  FDyeNames := TStringList.Create;
  LoadDyeNames;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FDyeNames.Free;
  FDyeEntries.Free;
end;

procedure TfrmMain.LoadDyeNames;
var
  filePath: string;
  i: Integer;
  lines: TStringList;
begin
  filePath := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), DYE_NAMES_FILE);
  if not FileExists(filePath) then
    filePath := TPath.Combine(GetCurrentDir, DYE_NAMES_FILE);

  FDyeNames.Clear;
  if FileExists(filePath) then
  begin
    lines := TStringList.Create;
    try
      lines.LoadFromFile(filePath);
      for i := 0 to lines.Count - 1 do
      begin
        if Trim(lines[i]) <> '' then
          FDyeNames.Add(Trim(lines[i]));
      end;
    finally
      lines.Free;
    end;
  end;
end;

procedure TfrmMain.SaveDyeNames;
var
  filePath: string;
begin
  filePath := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), DYE_NAMES_FILE);
  FDyeNames.SaveToFile(filePath);
end;

procedure TfrmMain.UpdateDyeNamesFromEntries;
var
  entry: TDyeEntry;
  hasNewNames: Boolean;
begin
  hasNewNames := False;
  for entry in FDyeEntries do
  begin
    if FDyeNames.IndexOf(entry.Color) < 0 then
    begin
      FDyeNames.Add(entry.Color);
      hasNewNames := True;
    end;
  end;

  if hasNewNames then
    SaveDyeNames;
end;

procedure TfrmMain.RefreshGrid;
var
  i: Integer;
  entry: TDyeEntry;
begin
  grdDyes.RowCount := FDyeEntries.Count;
  for i := 0 to FDyeEntries.Count - 1 do
  begin
    entry := FDyeEntries[i];
    grdDyes.Cells[0, i] := entry.Color;
    grdDyes.Cells[1, i] := FormatFloat('0.##', entry.AmountInEachFMO);
    if entry.Location = dlFMO then
      grdDyes.Cells[2, i] := LOCATION_MAKE_FMO
    else
      grdDyes.Cells[2, i] := LOCATION_COMMON_BACKBONE;
  end;
end;

procedure TfrmMain.btnAddClick(Sender: TObject);
var
  dlg: TdlgDyeEntry;
  entry: TDyeEntry;
begin
  dlg := TdlgDyeEntry.Create(Self);
  try
    dlg.Caption := 'Add Dye Entry';
    dlg.SetColorItems(FDyeNames);
    if dlg.ShowModal = mrOk then
    begin
      entry.Color := dlg.GetColor;
      entry.AmountInEachFMO := dlg.GetAmount;
      entry.Location := dlg.GetLocation;
      FDyeEntries.Add(entry);
      RefreshGrid;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TfrmMain.btnEditClick(Sender: TObject);
var
  dlg: TdlgDyeEntry;
  entry: TDyeEntry;
  selRow: Integer;
begin
  selRow := grdDyes.Row;
  if (selRow < 0) or (selRow >= FDyeEntries.Count) then
  begin
    ShowMessage('Please select a row to edit.');
    Exit;
  end;

  entry := FDyeEntries[selRow];

  dlg := TdlgDyeEntry.Create(Self);
  try
    dlg.Caption := 'Edit Dye Entry';
    dlg.SetColorItems(FDyeNames);
    dlg.SetColor(entry.Color);
    dlg.SetAmount(entry.AmountInEachFMO);
    dlg.SetLocation(entry.Location);
    if dlg.ShowModal = mrOk then
    begin
      entry.Color := dlg.GetColor;
      entry.AmountInEachFMO := dlg.GetAmount;
      entry.Location := dlg.GetLocation;
      FDyeEntries[selRow] := entry;
      RefreshGrid;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TfrmMain.btnDeleteClick(Sender: TObject);
var
  selRow: Integer;
begin
  selRow := grdDyes.Row;
  if (selRow < 0) or (selRow >= FDyeEntries.Count) then
  begin
    ShowMessage('Please select a row to delete.');
    Exit;
  end;

  FDyeEntries.Delete(selRow);
  RefreshGrid;
end;

procedure TfrmMain.btnClearClick(Sender: TObject);
begin
  FDyeEntries.Clear;
  RefreshGrid;
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
var
  outputLines: TStringList;
  entry: TDyeEntry;
  fmoStr: string;
begin
  if FDyeEntries.Count = 0 then
  begin
    ShowMessage('No entries to save.');
    Exit;
  end;

  if SaveDialog.Execute then
  begin
    outputLines := TStringList.Create;
    try
      outputLines.Add('dye,amount,FMO');
      for entry in FDyeEntries do
      begin
        if entry.Location = dlFMO then
          fmoStr := 'true'
        else
          fmoStr := 'false';
        outputLines.Add(Format('%s,%s,%s', [entry.Color,
          FormatFloat('0.##', entry.AmountInEachFMO), fmoStr]));
      end;
      outputLines.SaveToFile(SaveDialog.FileName);
      ShowMessage('CSV file saved successfully.');
    finally
      outputLines.Free;
    end;
  end;
end;

procedure TfrmMain.btnLoadClick(Sender: TObject);
var
  lines: TStringList;
  i: Integer;
  line: string;
  parts: TArray<string>;
  entry: TDyeEntry;
  amountVal: Double;
begin
  if OpenDialog.Execute then
  begin
    lines := TStringList.Create;
    try
      lines.LoadFromFile(OpenDialog.FileName);
      FDyeEntries.Clear;

      for i := 0 to lines.Count - 1 do
      begin
        line := Trim(lines[i]);
        if line = '' then
          Continue;

        // Skip header row
        if SameText(line, 'dye,amount,FMO') then
          Continue;

        parts := line.Split([',']);
        if Length(parts) >= 3 then
        begin
          entry.Color := Trim(parts[0]);
          if TryStrToFloat(Trim(parts[1]), amountVal) then
            entry.AmountInEachFMO := amountVal
          else
            entry.AmountInEachFMO := 0;
          if SameText(Trim(parts[2]), 'true') then
            entry.Location := dlFMO
          else
            entry.Location := dlCommon;
          FDyeEntries.Add(entry);
        end;
      end;

      RefreshGrid;
    finally
      lines.Free;
    end;
  end;
end;

procedure TfrmMain.btnGenerateRecipeClick(Sender: TObject);
var
  recipe: TFMORecipeSimple;
begin
  if FDyeEntries.Count < 3 then
  begin
    ShowMessage('At least 3 dyes are required to generate a recipe.');
    Exit;
  end;

  UpdateDyeNamesFromEntries;

  recipe := TFMORecipeSimple.Create;
  try
    try
      recipe.BuildRecipe(FDyeEntries);
      mmoRecipe.Lines.Clear;
      recipe.GetRecipeText(mmoRecipe.Lines);
    except
      on E: Exception do
        ShowMessage('Error generating recipe: ' + E.Message);
    end;
  finally
    recipe.Free;
  end;
end;

procedure TfrmMain.grdDyesCellDblClick(const Column: TColumn; const Row:
    Integer);
begin
  if (Row >= 0) and (Row < FDyeEntries.Count) then
    btnEditClick(nil);

end;

procedure TfrmMain.grdDyesSelectCell(Sender: TObject; const ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  CanSelect := True;
end;

procedure TfrmMain.mnuFileSaveClick(Sender: TObject);
begin
  btnSaveClick(Sender);
end;

procedure TfrmMain.mnuFileLoadClick(Sender: TObject);
begin
  btnLoadClick(Sender);
end;

procedure TfrmMain.mnuFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.mnuFileSaveRecipeClick(Sender: TObject);
begin
  if mmoRecipe.Lines.Count = 0 then
  begin
    ShowMessage('No recipe to save. Please build a recipe first.');
    Exit;
  end;

  if RecipeSaveDialog.Execute then
  begin
    mmoRecipe.Lines.SaveToFile(RecipeSaveDialog.FileName);
    ShowMessage('Recipe saved successfully.');
  end;
end;

procedure TfrmMain.mnuBuildRecipeClick(Sender: TObject);
begin
  btnGenerateRecipeClick(Sender);
end;

end.
