program FMORecipeBuilder;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  DyeEntryDialog in 'DyeEntryDialog.pas' {dlgDyeEntry},
  FMORecipe.Types in 'FMORecipe.Types.pas',
  FMORecipe.Sets in 'FMORecipe.Sets.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
