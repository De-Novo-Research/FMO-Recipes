unit FMORecipe.Sets;

interface

uses
  System.SysUtils,
  System.Generics.Defaults,
  System.Classes,
  System.Generics.Collections,
  FMORecipe.Types;

type
  TSetType = (stElement, stCommonIntermediate, stCommonFinal, stForward, stBackward, stFMO);

  IElement = interface;
  ISet = interface;

  IRecipeItem=interface
    ['{8861D94D-FCF8-4DA6-AD97-6B005B5C228E}']
    function GetName: string;
    property Name: string read GetName;
    function GetTotalQuantity: Double;
  end;

  IElement = interface(IRecipeItem)
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function GetAmountInEachFMO: Double;

    property AmountInEachFMO: Double read GetAmountInEachFMO;
  end;

  ISet = interface(IRecipeItem)
    ['{B2C3D4E5-F6A7-8901-BCDE-F12345678901}']
    function GetLeftInput: ISet;
    procedure SetLeftInput(const aValue: ISet);
    function GetRightInput: ISet;
    procedure SetRightInput(const aValue: ISet);
    procedure PopulateElements(const aList: TList<IElement>);
    procedure AddQuantities(const aQuantities: TDictionary<IElement, Double>);
    function GetQuantityFor(const aElement: IElement): Double;
    function GetLeftInputQuantity: Double;
    function GetRightInputQuantity: Double;
    function GetSetType: TSetType;
    property LeftInput: ISet read GetLeftInput write SetLeftInput;
    property RightInput: ISet read GetRightInput write SetRightInput;
    property SetType: TSetType read GetSetType;
  end;

  TElement = class(TInterfacedObject, IElement, ISet)
  private
    FName: string;
    FAmountInEachFMO: Double;
    FAccumulatedQuantity: Double;
  protected
    // IElement
    function GetName: string;
    function GetAmountInEachFMO: Double;
    // ISet
    function GetLeftInput: ISet;
    procedure SetLeftInput(const aValue: ISet);
    function GetRightInput: ISet;
    procedure SetRightInput(const aValue: ISet);
    procedure PopulateElements(const aList: TList<IElement>);
    procedure AddQuantities(const aQuantities: TDictionary<IElement, Double>);
    function GetTotalQuantity: Double;
    function GetQuantityFor(const aElement: IElement): Double;
    function GetLeftInputQuantity: Double;
    function GetRightInputQuantity: Double;
    function GetSetType: TSetType;
  public
    constructor Create(const aName: string; aAmountInEachFMO: Double);
    property Name: string read GetName;
    property AmountInEachFMO: Double read GetAmountInEachFMO;
  end;

  TSet = class(TInterfacedObject, ISet)
  private
    FName: string;
    FSetType: TSetType;
    FLeftInput: ISet;
    FRightInput: ISet;
    FLeftElements: TList<IElement>;
    FQuantities: TDictionary<IElement, Double>;
  protected
    function GetName: string;
    function GetLeftInput: ISet;
    procedure SetLeftInput(const aValue: ISet);
    function GetRightInput: ISet;
    procedure SetRightInput(const aValue: ISet);
    procedure PopulateElements(const aList: TList<IElement>);
    procedure AddQuantities(const aQuantities: TDictionary<IElement, Double>);
    function GetTotalQuantity: Double;
    function GetQuantityFor(const aElement: IElement): Double;
    function GetLeftInputQuantity: Double;
    function GetRightInputQuantity: Double;
    function GetSetType: TSetType;
  public
    constructor Create(const aName: string; aSetType: TSetType; const aLeft, aRight: ISet);
    destructor Destroy; override;
    property Name: string read GetName;
    property SetType: TSetType read GetSetType;
    property LeftInput: ISet read GetLeftInput write SetLeftInput;
    property RightInput: ISet read GetRightInput write SetRightInput;
  end;

  TFMORecipeSimple = class
  private
    FElements: TList<IElement>;
    FLeaveOutElements: TList<IElement>;
    FCommonElements: TList<IElement>;
    FFinalSets: TList<ISet>;
    procedure CollectSets(const aSet: ISet; const aVisited: TList<ISet>; const aResult: TList<ISet>);
  public
    constructor Create;
    destructor Destroy; override;

    procedure BuildRecipe(const aDyeEntries: TDyeEntryList);
    procedure GetRecipeText(aOutput: TStrings);
    procedure SaveToTextFile(const aFileName: string);

    property Elements: TList<IElement> read FElements;
    property LeaveOutElements: TList<IElement> read FLeaveOutElements;
    property CommonElements: TList<IElement> read FCommonElements;
    property FinalSets: TList<ISet> read FFinalSets;
  end;

implementation


{ TElement }

constructor TElement.Create(const aName: string; aAmountInEachFMO: Double);
begin
  inherited Create;
  FName := aName;
  FAmountInEachFMO := aAmountInEachFMO;
end;

function TElement.GetName: string;
begin
  Result := FName;
end;

function TElement.GetAmountInEachFMO: Double;
begin
  Result := FAmountInEachFMO;
end;

function TElement.GetLeftInput: ISet;
begin
  Result := nil;
end;

procedure TElement.SetLeftInput(const aValue: ISet);
begin
  // Elements don't have inputs - no-op
end;

function TElement.GetRightInput: ISet;
begin
  Result := nil;
end;

procedure TElement.SetRightInput(const aValue: ISet);
begin
  // Elements don't have inputs - no-op
end;

procedure TElement.PopulateElements(const aList: TList<IElement>);
begin
  aList.Add(Self);
end;

procedure TElement.AddQuantities(const aQuantities: TDictionary<IElement, Double>);
var
  amount: Double;
begin
  if aQuantities.TryGetValue(Self, amount) then
    FAccumulatedQuantity := FAccumulatedQuantity + amount;
end;

function TElement.GetTotalQuantity: Double;
begin
  Result := FAccumulatedQuantity;
end;

function TElement.GetQuantityFor(const aElement: IElement): Double;
begin
  if aElement = (Self as IElement) then
    Result := FAccumulatedQuantity
  else
    Result := 0;
end;

function TElement.GetLeftInputQuantity: Double;
begin
  Result := 0; // Elements have no inputs
end;

function TElement.GetRightInputQuantity: Double;
begin
  Result := 0; // Elements have no inputs
end;

function TElement.GetSetType: TSetType;
begin
  Result := stElement;
end;

{ TSet }

constructor TSet.Create(const aName: string; aSetType: TSetType; const aLeft, aRight: ISet);
begin
  inherited Create;
  FName := aName;
  FSetType := aSetType;
  FLeftInput := aLeft;
  FRightInput := aRight;
  FQuantities := TDictionary<IElement, Double>.Create;
  // Cache which elements belong to the left input for quantity splitting
  FLeftElements := TList<IElement>.Create;
  FLeftInput.PopulateElements(FLeftElements);
end;

destructor TSet.Destroy;
begin
  FLeftElements.Free;
  FQuantities.Free;
  inherited;
end;

function TSet.GetName: string;
begin
  Result := FName;
end;

function TSet.GetLeftInput: ISet;
begin
  Result := FLeftInput;
end;

procedure TSet.SetLeftInput(const aValue: ISet);
begin
  FLeftInput := aValue;
end;

function TSet.GetRightInput: ISet;
begin
  Result := FRightInput;
end;

procedure TSet.SetRightInput(const aValue: ISet);
begin
  FRightInput := aValue;
end;

procedure TSet.PopulateElements(const aList: TList<IElement>);
begin
  if Assigned(FLeftInput) then
    FLeftInput.PopulateElements(aList);
  if Assigned(FRightInput) then
    FRightInput.PopulateElements(aList);
end;

procedure TSet.AddQuantities(const aQuantities: TDictionary<IElement, Double>);
var
  leftQuantities, rightQuantities: TDictionary<IElement, Double>;
  pair: TPair<IElement, Double>;
  currentQty: Double;
begin
  // Accumulate quantities in this set
  for pair in aQuantities do
  begin
    if FQuantities.TryGetValue(pair.Key, currentQty) then
      FQuantities[pair.Key] := currentQty + pair.Value
    else
      FQuantities.Add(pair.Key, pair.Value);
  end;

  // Split quantities between left and right inputs
  leftQuantities := TDictionary<IElement, Double>.Create;
  rightQuantities := TDictionary<IElement, Double>.Create;
  try
    for pair in aQuantities do
    begin
      if FLeftElements.Contains(pair.Key) then
        leftQuantities.Add(pair.Key, pair.Value)
      else
        rightQuantities.Add(pair.Key, pair.Value);
    end;

    // Propagate to children
    if leftQuantities.Count > 0 then
      FLeftInput.AddQuantities(leftQuantities);
    if rightQuantities.Count > 0 then
      FRightInput.AddQuantities(rightQuantities);
  finally
    rightQuantities.Free;
    leftQuantities.Free;
  end;
end;

function TSet.GetTotalQuantity: Double;
begin
  Result := 0;
  for var qty in FQuantities.Values do
    Result := Result + qty;
end;

function TSet.GetQuantityFor(const aElement: IElement): Double;
begin
  if not FQuantities.TryGetValue(aElement, Result) then
    Result := 0;
end;

function TSet.GetLeftInputQuantity: Double;
var
  elem: IElement;
begin
  Result := 0;
  for elem in FLeftElements do
    Result := Result + GetQuantityFor(elem);
end;

function TSet.GetRightInputQuantity: Double;
var
  pair: TPair<IElement, Double>;
begin
  Result := 0;
  for pair in FQuantities do
    if not FLeftElements.Contains(pair.Key) then
      Result := Result + pair.Value;
end;

function TSet.GetSetType: TSetType;
begin
  Result := FSetType;
end;

{ TFMORecipeSimple }

constructor TFMORecipeSimple.Create;
begin
  inherited;
  FElements := TList<IElement>.Create;
  FLeaveOutElements := TList<IElement>.Create;
  FCommonElements := TList<IElement>.Create;
  FFinalSets := TList<ISet>.Create;
end;

destructor TFMORecipeSimple.Destroy;
begin
  FFinalSets.Free;
  FCommonElements.Free;
  FLeaveOutElements.Free;
  FElements.Free;
  inherited;
end;

procedure TFMORecipeSimple.CollectSets(const aSet: ISet; const aVisited: TList<ISet>; const aResult: TList<ISet>);
begin
  if aVisited.Contains(aSet) then
    Exit;
  aVisited.Add(aSet);

  // Only collect TSet instances (not TElement)
  if not Supports(aSet, IElement) or (aSet.LeftInput <> nil) then
  begin
    // Recurse into children first (post-order)
    if aSet.LeftInput <> nil then
      CollectSets(aSet.LeftInput, aVisited, aResult);
    if aSet.RightInput <> nil then
      CollectSets(aSet.RightInput, aVisited, aResult);

    // Only add if this is a TSet (has inputs)
    if aSet.LeftInput <> nil then
      aResult.Add(aSet);
  end;
end;

procedure TFMORecipeSimple.BuildRecipe(const aDyeEntries: TDyeEntryList);
const
  FMO_NAME_FMT = 'FMO (-%s)';
var
  sortedEntries: TDyeEntryList;
  entry: TDyeEntry;
  elem: IElement;
  i, j, m: Integer;
  commonUnion: ISet;
  forwardChain: TList<ISet>;
  backwardChain: TList<ISet>;
  finalSetQuantities: TDictionary<IElement, Double>;
  finalSet: ISet;
  finalSetElements: TList<IElement>;
begin
  // Clear previous data
  FElements.Clear;
  FLeaveOutElements.Clear;
  FCommonElements.Clear;
  FFinalSets.Clear;

  // Sort entries alphabetically by color
  sortedEntries := TDyeEntryList.Create;
  try
    sortedEntries.AddRange(aDyeEntries.ToArray);
    sortedEntries.Sort(TComparer<TDyeEntry>.Construct(
      function(const Left, Right: TDyeEntry): Integer
      begin
        Result := CompareText(Left.Color, Right.Color);
      end));

    // Create TElement objects for each dye
    for entry in sortedEntries do
    begin
      elem := TElement.Create(entry.Color, entry.AmountInEachFMO);
      FElements.Add(elem);

      if entry.Location = dlFMO then
        FLeaveOutElements.Add(elem)
      else
        FCommonElements.Add(elem);
    end;
  finally
    sortedEntries.Free;
  end;

  m := FLeaveOutElements.Count;
  if m < 3 then
    raise Exception.Create('At least 3 dyes must be marked "leave out".');

  // Build common union (C) from common elements
  commonUnion := nil;
  if FCommonElements.Count > 0 then
  begin
    commonUnion := FCommonElements[0] as ISet;
    for i := 1 to FCommonElements.Count - 1 do
    begin
      if i = FCommonElements.Count - 1 then
        commonUnion := TSet.Create('Common', stCommonFinal, commonUnion, FCommonElements[i] as ISet)
      else
        commonUnion := TSet.Create('C' + IntToStr(i), stCommonIntermediate, commonUnion, FCommonElements[i] as ISet);
    end;
  end;

  // Build forward chain: FC[i] = C + L[0] + L[1] + ... + L[i-1]
  forwardChain := TList<ISet>.Create;
  backwardChain := TList<ISet>.Create;
  try
    // F1 = C + L[0]
    if Assigned(commonUnion) then
      forwardChain.Add(TSet.Create('F1', stForward, commonUnion, FLeaveOutElements[0] as ISet))
    else
      forwardChain.Add(FLeaveOutElements[0] as ISet);

    // F[i] = F[i-1] + L[i]
    // Note: The last forward chain (i = m-2) is already the FMO missing L[M-1]
    for i := 1 to m - 2 do
    begin
      if i = m - 2 then
        forwardChain.Add(TSet.Create(Format(FMO_NAME_FMT, [FLeaveOutElements[m - 1].Name]),
          stFMO, forwardChain[i - 1], FLeaveOutElements[i] as ISet))
      else
        forwardChain.Add(TSet.Create('F' + IntToStr(i + 1),
          stForward, forwardChain[i - 1], FLeaveOutElements[i] as ISet));
    end;

    // Build backward chain: B[i] = L[i+1] + L[i+2] + ... + L[M-1]
    // B[M-2] = L[M-1]
    backwardChain.Add(FLeaveOutElements[m - 1] as ISet);

    // B[i] = L[i+1] + B[i+1]
    // Note: When no common union, B1 (i=0) is already the FMO missing L[0]
    for i := m - 3 downto 0 do
    begin
      if (i = 0) and not Assigned(commonUnion) then
        backwardChain.Insert(0, TSet.Create(Format(FMO_NAME_FMT, [FLeaveOutElements[0].Name]),
          stFMO, FLeaveOutElements[1] as ISet, backwardChain[0]))
      else
        backwardChain.Insert(0, TSet.Create('B' + IntToStr(i + 1),
          stBackward, FLeaveOutElements[i + 1] as ISet, backwardChain[0]));
    end;

    // Build final sets (one per leave-out element)
    // S[0] = C + B[0]  (everything except L[0])
    // When no common union, backwardChain[0] is already named as FMO
    if Assigned(commonUnion) then
      FFinalSets.Add(TSet.Create(Format(FMO_NAME_FMT, [FLeaveOutElements[0].Name]), stFMO, commonUnion, backwardChain[0]))
    else
      FFinalSets.Add(backwardChain[0]);

    // S[i] = F[i-1] + B[i]  for i = 1 to M-2
    for i := 1 to m - 2 do
      FFinalSets.Add(TSet.Create(Format(FMO_NAME_FMT, [FLeaveOutElements[i].Name]), stFMO, forwardChain[i - 1], backwardChain[i]));

    // S[M-1] = F[M-2]  (everything except L[M-1])
    // Already named as FMO when created in forward chain
    FFinalSets.Add(forwardChain[m - 2]);

  finally
    backwardChain.Free;
    forwardChain.Free;
  end;

  // Populate quantities for each final set
  for i := 0 to FFinalSets.Count - 1 do
  begin
    finalSet := FFinalSets[i];
    finalSetElements := TList<IElement>.Create;
    try
      finalSet.PopulateElements(finalSetElements);
      finalSetQuantities := TDictionary<IElement, Double>.Create;
      try
        for j := 0 to finalSetElements.Count - 1 do
        begin
          elem := finalSetElements[j];
          finalSetQuantities.Add(elem, elem.AmountInEachFMO);
        end;
        finalSet.AddQuantities(finalSetQuantities);
      finally
        finalSetQuantities.Free;
      end;
    finally
      finalSetElements.Free;
    end;
  end;
end;

procedure TFMORecipeSimple.GetRecipeText(aOutput: TStrings);
var
  elem: IElement;
  aSet: ISet;
  allSets: TList<ISet>;
  visited: TList<ISet>;
  i: Integer;
  elements: TList<IElement>;
  qty: Double;
begin
  allSets := TList<ISet>.Create;
  visited := TList<ISet>.Create;
  elements := TList<IElement>.Create;
  try
    aOutput.Add('FMO RECIPE');
    aOutput.Add('==========');
    aOutput.Add('');

    // Input Dyes section - common first, then FMO
    aOutput.Add('Final Dye Amount Per FMO:');
    aOutput.Add('-----------');
    for elem in FCommonElements do
      aOutput.Add(Format('  %s: %.2f uL (common)', [elem.Name, elem.AmountInEachFMO]));
    for elem in FLeaveOutElements do
      aOutput.Add(Format('  %s: %.2f uL (FMO)', [elem.Name, elem.AmountInEachFMO]));
    aOutput.Add('');

    // Collect all mixing steps in dependency order
    for aSet in FFinalSets do
      CollectSets(aSet, visited, allSets);


    // Required Reagents steps section
    aOutput.Add('Required Reagents:');
    aOutput.Add('-------------');
    aOutput.Add('');

    for elem in FElements do
    begin
      qty := elem.GetTotalQuantity;
      if qty > 0 then
        aOutput.Add(Format('  %s: %.2f uL', [elem.Name, qty]));
    end;

    var tubeCount := 0;
    for aSet in allSets do
      if aSet.SetType <> stCommonIntermediate then
        Inc(tubeCount);
    aOutput.Add(Format('  %d tubes', [tubeCount]));
    aOutput.Add('');

    // Labelling Steps section
    aOutput.Add('Labelling Steps:');
    aOutput.Add('-------------');
    aOutput.Add('');

    var sortedSets: TArray<ISet>;
    SetLength(sortedSets, allSets.Count);
    for i := 0 to allSets.Count - 1 do
      sortedSets[i] := allSets[i];

    TArray.Sort<ISet>(sortedSets, TComparer<ISet>.Construct(
      function(const Left, Right: ISet): Integer
      const
        // Order: CommonFinal=1, Forward=2, Backward=3, FMO=4 (CommonIntermediate=0 filtered out)
        TYPE_ORDER: array[TSetType] of Integer = (0, 0, 1, 2, 3, 4);
      var
        order1, order2: Integer;
      begin
        order1 := TYPE_ORDER[Left.SetType];
        order2 := TYPE_ORDER[Right.SetType];
        if order1 <> order2 then
          Result := order1 - order2
        else
          Result := CompareText(Left.Name, Right.Name);
      end));

    var stepNum := 1;
    for var sortedSet in sortedSets do
    begin
      // Skip intermediate common tubes - user just makes Common directly
      if sortedSet.SetType = stCommonIntermediate then
        Continue;
      aOutput.Add(Format('  Step %d: Label a tube as "%s"', [stepNum, sortedSet.Name]));
      Inc(stepNum);
    end;

    // Mixing Steps section
    aOutput.Add('');
    aOutput.Add('Mixing Steps:');
    aOutput.Add('-------------');

    stepNum := 1;
    for aSet in allSets do
    begin
      // Skip intermediate common tubes - Common tube instruction handles all common elements
      if aSet.SetType = stCommonIntermediate then
        Continue;

      aOutput.Add('');

      if aSet.SetType = stCommonFinal then
      begin
        // Special handling for Common tube - list all common elements
        aOutput.Add(Format('  Step %d: Add to tube %s:', [stepNum, aSet.Name]));
        for elem in FCommonElements do
          aOutput.Add(Format('    %s: %.2f uL', [elem.Name, elem.GetTotalQuantity]));
      end
      else
      begin
        aOutput.Add(Format('  Step %d: Mix: %s (%.2f uL) + %s (%.2f uL) into tube %s',
          [stepNum, aSet.LeftInput.Name, aSet.GetLeftInputQuantity,
           aSet.RightInput.Name, aSet.GetRightInputQuantity, aSet.Name]));
        // Show composition
        aOutput.Add('  Contains:');
        elements.Clear;
        aSet.PopulateElements(elements);
        for elem in elements do
          aOutput.Add(Format('    %s: %.2f uL', [elem.Name, aSet.GetQuantityFor(elem)]));
      end;

      Inc(stepNum);
    end;

    aOutput.Add('');

    // FMO Control Tubes section
    aOutput.Add('FMO Control Tubes:');
    aOutput.Add('------------------');

    for i := 0 to FFinalSets.Count - 1 do
    begin
      aSet := FFinalSets[i];
      aOutput.Add(Format('  %s:', [aSet.Name]));

      elements.Clear;
      aSet.PopulateElements(elements);
      for elem in elements do
        aOutput.Add(Format('    %s: %.2f uL', [elem.Name, aSet.GetQuantityFor(elem)]));

      aOutput.Add('');
    end;


  finally
    elements.Free;
    visited.Free;
    allSets.Free;
  end;
end;

procedure TFMORecipeSimple.SaveToTextFile(const aFileName: string);
var
  output: TStringList;
begin
  output := TStringList.Create;
  try
    GetRecipeText(output);
    output.SaveToFile(aFileName);
  finally
    output.Free;
  end;
end;

end.
