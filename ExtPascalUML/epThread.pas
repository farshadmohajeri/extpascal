unit epThread; {$I epDirectives.inc}

interface

uses
  FCGIApp, epObjectList, epPrevalence, epCommon, epUtils, epProperties,
  ExtPascal, Ext, ExtGlobal, ExtGrid, ExtTree, ExtData, ExtMenu, ExtForm;

const
  ResponseTimeout = 60000;

type
  TUserInfo = record
    UserDomain,
    UserName,
    UserFullName,
    ComputerName,
    LogonSrv,
    Profile,
    Profiles,
    RunningMethodName : string;
  end;

  TepThread = class(TExtThread)
  private
    FThreadTrans: TTransaction;
    TaskMenu    : TExtMenuMenu;
    CentralPanel: TExtTabPanel;
    InitialPage : TExtPanel;
    Enums, InvalidMessage : string;
    InvalidField, FormWidth, MaxHeader, Pad : integer;
    Editors : array of TExtFormField;
    function LoadUserInfo : boolean;
    function ClassTreePanel : TExtTreeTreePanel;
    procedure LoadTaskMenu;
    function CreateEditor(Props: TProperties; I: integer; var EditorLength : integer): TExtFormField;
    procedure DeleteSelection;
    procedure DeleteSelections;
    procedure AddFieldsToForm(Items : TExtObjectList; var HasGroup : boolean);
  protected
    function BeforeHandleRequest : boolean; override;
    procedure AfterHandleRequest; override;
  public
    GDB : pointer;
    Action   : TBrowserButton;
    UserInfo : TUserInfo;
    ConnectionDateTime : TDateTime;
    EditorGrid : TExtGridEditorGridPanel;
    Selection : TExtGridCheckboxSelectionModel;
    GridPanel : TExtPanel;
    RecordForm : TExtUxGridRecordForm;
    DataStore : TExtDataJsonStore;
    PrevalentList : TPrevalentList;
    InitialProp : integer;
    Props : TProperties;
    function Confirm(Msg : string; TimeOut : integer = ResponseTimeout) : boolean;
    function Edit(Obj : TPrevalent; PropOrder : string = ''; Title : string = ''; Message : string = ''; Buttons : TBrowserButtons = [bbNext, bbCancel]; ReadOnly : boolean = false; TimeOut : integer = ResponseTimeout) : TBrowserButton; overload;
    function Edit(List : TObjectList; PropOrder : string = ''; Title : string = ''; Message: string = ''; Buttons: TBrowserButtons = [bbNext, bbCancel]; Format : TBrowserEditFormat = beGrid; const ReadOnly : boolean = false; TimeOut : integer = ResponseTimeout): TBrowserButton; overload;
    function Choose(List : TObjectList; PropOrder : string = ''; Title : string = ''; Message : string = ''; Buttons: TBrowserButtons = [bbNext, bbCancel];
                    TimeOut : integer = ResponseTimeout) : TTransient; overload;
    function Choose(List: TObjectList; Options: array of string; PropOrder: string = ''; Title: string = ''; Message: string = ''; Buttons: TBrowserButtons = [bbNext, bbCancel];
                    TimeOut: integer = ResponseTimeout) : TTransient; overload;
    function MessageDlg(Msg : string; DlgType : TMsgDlgType = mtInformation; Buttons : TMsgDlgButtons = [mbOk]; TimeOut : integer = ResponseTimeout) : integer;
    procedure Message(Msg: string; Clear: boolean = false); // Async show message
    procedure Error(Message : string);
    procedure ShowMessage(Message : string);
  published
    procedure Home; override;
    procedure BrowseClass;
    procedure ShowInitialPage;
    procedure FreeInitialPage;
    procedure OpenObjects;
    procedure LoadData;
    procedure Commit;
    procedure Cancel;
    procedure AddObject;
    procedure DeleteObject;
    procedure EditObject;
    procedure EditRecordForm;
    procedure UpdateObject;
    procedure ValidateField;
    procedure About;
    procedure Mark;
  end;

function Browser : TepThread;

implementation

uses
  SysUtils, StrUtils, Classes, Math, TypInfo, epServer,
  ExtPascalUtils, ExtUtil, ExtState;

function Browser : TepThread; begin
  Result := TepThread(CurrentFCGIThread)
end;

procedure TepThread.About; begin
  with TExtWindow.Create do begin
    Title  := Application.Title;
    Layout := lyBorder;
    Width  := 317;
    Height := 200;
    Modal  := true;
    Resizable := false;
    with TExtPanel.AddTo(Items) do begin
      Region := rgWest;
      Border := false;
      Html   := '<img src=' + ImagePath + '/extpascal.png />';
    end;
    with TExtPanel.AddTo(Items) do begin
      Region := rgCenter;
      Border := false;
      Html   := '<br/><img src=' + ImagePath + '/logofreepascal.jpg /><br/><br/><img src=' + ImagePath + '/logoturbodelphi.jpg />';
    end;
    Show;
    Free;
  end;
end;

function TepThread.BeforeHandleRequest : boolean; begin
  Result := inherited BeforeHandleRequest;
  ThreadTrans := FThreadTrans;
end;

procedure TepThread.AfterHandleRequest; begin
  inherited;
  FThreadTrans := ThreadTrans
end;

procedure TepThread.FreeInitialPage; begin
  FreeAndNil(InitialPage);
end;

procedure TepThread.ShowInitialPage; begin
  if InitialPage = nil then begin
    InitialPage := TExtPanel.Create;
    with InitialPage.AddTo(CentralPanel.Items) do begin
      Id         := 'InitialPage';
      Title      := 'ExtPascalUML';
      IconCls    := 'info';
      Closable   := true;
      AutoScroll := true;
      AutoLoadString := ImagePath + '/pitinnu.html';
      on('destroy', Ajax(FreeInitialPage));
    end;
  end;
  CentralPanel.Activate('InitialPage');
end;

function TepThread.LoadUserInfo : boolean; begin
  Result := true;
  if NewThread then begin
    if RequestHeader['AUTH_TYPE'] <> '' then ; // epServer.AuthenticateUser; carregar demais campos UserInfo
    UserInfo.ComputerName := RequestHeader['REMOTE_ADDR'];
    UserInfo.UserName     := RequestHeader['REMOTE_USER'];
    ConnectionDateTime    := Now;
    Login(Self);
  end;
end;

function IsHidden(Alias : string) : boolean; begin
  Result := pos('hidden', lowercase(Alias)) <> 0
end;

procedure TepThread.LoadTaskMenu;
var
  I, J, K, MaxMenu : integer;
  Tasks, Options : TStringList;
  Item : TExtMenuItem;
  Menu : array of record
    Option : string;
    Menu   : TExtMenuMenu;
  end;
begin
  TaskMenu := TExtMenuMenu.Create;
  Tasks := TStringList.Create;
  Tasks.Sorted := true;
  Tasks.Duplicates := dupIgnore;
  MaxMenu := 0;
  with Prevalence do
    for I := 0 to Prevalents.Count - 1 do
      with Metadata(Prevalents[I]) do
        for J := 0 to MethodCount - 1 do
          with Method[J]^ do
            if (Stereotype in [_stWORKFLOW, _stWIZARD]) and not IsHidden(Alias) then begin
              Tasks.Add(Alias);
              MaxMenu := max(CountStr('|', Alias),MaxMenu);
            end;
  Options := TStringList.Create;
  Options.Delimiter := '|';
  Options.StrictDelimiter := true;
  SetLength(Menu, MaxMenu+1);
  Menu[0].Option := '';
  Menu[0].Menu   := TaskMenu;
  Item := nil;
  for I := 0 to Tasks.Count-1 do begin
    Options.DelimitedText := Tasks[I];
    for J := 0 to Options.Count-1 do
      if Menu[J].Option <> Options[J] then begin
        for K := J+1 to MaxMenu do begin
          Menu[K].Option := '';
          Menu[K].Menu   := nil;
        end;
        Menu[J].Option := Options[J];
        if Menu[J].Menu = nil then begin
          Menu[J].Menu := TExtMenuMenu.Create;
          Item.Menu := Menu[J].Menu;
        end;
        Item := TExtMenuItem.AddTo(Menu[J].Menu.Items);
        Item.Text := Options[J];
      end;
  end;
  Options.Free;
  Tasks.Free;
  with TaskMenu do begin
    AddSeparator;
    Item := TExtMenuItem.Create;
    with Item do begin
      Text    := 'Sair';
      Handler := Ajax(Logout);
      IconCls := 'exit';
    end;
    AddItem(Item);
  end;
end;

function TepThread.ClassTreePanel : TExtTreeTreePanel;

  function GetParents(Parent : TClass) : string;
  var
    Alias : string;
  begin
    Result := '';
    while Parent <> TPrevalent do begin
      Alias := Prevalence.Metadata(Parent.ClassName).AliasProp[0];
      if not IsHidden(Alias) then Result := Alias + '|' + Result;
      Parent := Parent.ClassParent;
    end;
  end;

var
  I, J, K, MaxTree, PrevalentNumber : integer;
  Alias : string;
  PrevalentList : TPrevalentList;
  Classes, Parents : TStringList;
  Tree : array of TExtTreeTreeNode;
begin
  Result := TExtTreeTreePanel.Create;
  with Result do begin
    Region  := rgWest;
    Title   := 'Classes';
    Split   := true;
    Width   := 200;
    Collapsible := true;
    AutoScroll  := true;
    RootVisible := false;
    Root := TExtTreeTreeNode.Create;
    Root.Expanded := true;
  end;
  Classes := TStringList.Create;
  Classes.Sorted := true;
  Classes.Duplicates := dupIgnore;
  MaxTree := 0;
  with Prevalence do
    for I := 0 to Prevalents.Count - 1 do begin
      PrevalentList := PrevalentLists(Prevalents[I] + 'List');
      PrevalentNumber := I;
      if PrevalentList = nil then begin
        PrevalentList := AbstractLists(Prevalents[I] + 'List');
        PrevalentNumber := -PrevalentNumber;
      end;
      Alias := Metadata(Prevalents[I]).AliasProp[0];
      {CName := copy(PrevalentList.ObjectClass.ClassName, 2, MaxInt);
      if pos('hidden', lowercase(Alias)) = 0 then
          ClassPerm := GetClassePerm(Props.Package, CName);
          ClassVisible := (ClassPerm * [_cpShow, _cpInsert] <> []) and (ClassPerm * [_cpHide] = []) and (CName <> '_Task');
        end;}
      if pos('|', Alias) = 0 then Alias := Metadata(Prevalents[I]).Package + '|' + GetParents(PrevalentList.ObjectClass.ClassParent) + Alias;
      Classes.AddObject(Alias, pointer(PrevalentNumber));
      MaxTree := max(CountStr('|', Alias), MaxTree);
    end;
  Parents := TStringList.Create;
  Parents.Delimiter := '|';
  Parents.StrictDelimiter := true;
  SetLength(Tree, MaxTree+2);
  Tree[0] := Result.Root_;
  for I := 0 to Classes.Count-1 do begin
    Parents.DelimitedText := Classes[I];
    for J := 0 to Parents.Count-1 do begin
      K := J + 1;
      if (Tree[K] = nil) or (Tree[K].Text <> Parents[J]) then begin
        Tree[K] := TExtTreeTreeNode.Create;
        with Tree[K] do begin
          Text := Parents[J];
          PrevalentNumber := integer(Classes.Objects[I]);
          IconCls := IfThen(K = 1, 'package', IfThen(PrevalentNumber < 0, 'abstract', 'objects'));
          if PrevalentNumber >= 0 then
            On('click', Ajax(BrowseClass, ['Class', abs(PrevalentNumber), 'GridHeight', GridPanel.GetInnerHeight]));
        end;
        Tree[K-1].AppendChild(Tree[K]);
      end;
    end;
  end;
  Parents.Free;
  Classes.Free;
end;

function PascalTypeToJS(Prop : PPropInfo) : string; begin
  with Prop.PropType^{$IFNDEF FPC}^{$ENDIF} do
    case Kind of
      tkInteger, tkInt64 : Result := 'int';
      tkString, tkSet, tkLString, tkChar, tkWChar, tkWString, tkVariant : Result := 'string';
      tkFloat :
        if GetDateTimeType(Name) = dtNone then
          Result := 'float'
        else
          Result := 'date';
      tkEnumeration :
        if lowercase(Name) = 'boolean' then
          Result := 'boolean'
        else
          Result := 'int';
      {$IFDEF FPC}tkBool : Result := 'boolean';{$ENDIF}
    else
      Result := 'auto'; // class
    end;
end;

function ClearEnumName(S : string) : string;
var
  I : integer;
  C : boolean;
begin
  C := false;
  Result := '';
  for I := 1 to length(S) do begin
    if S[I] in ['A'..'Z'] then
      if C then
        Result := Result + ' '
      else
        C := true;
    if C then Result := Result + S[I];
  end;
  Result := trim(Result);
end;

function GetMaskWidth(Mask : string) : integer;
var
  I, J : integer;
begin
  I := pos(' ', Mask);
  if I <> 0 then begin
    J := posex(',', Mask, I+1);
    if J = 0 then J := length(Mask);
    Result := StrToIntDef(trim(copy(Mask, I+1, J-I-1)), 30)
  end
  else
    Result := 30;
end;

function GetMaskHeight(Mask : string) : integer;
var
  I : integer;
begin
  I := pos(',', Mask);
  if I <> 0 then
    Result := StrToIntDef(trim(copy(Mask, I+1, length(Mask)-I)), 3)
  else
    Result := 3;
end;

function TepThread.CreateEditor(Props : TProperties; I : integer; var EditorLength : integer) : TExtFormField;
var
  Mask, Enum : string;
  J : integer;
  CompType, PropType : PTypeInfo;
begin
  EditorLength := 0;
  with Props do begin
    Mask := MaskProp[I];
    case CaseOf(PascalTypeToJS(Properties[I]), ['int', 'float', 'boolean', 'date', 'auto', 'string']) of
      0, 1 : // number
        if TypeProp[I] = tkEnumeration then begin
          Result := TExtFormComboBox.Create;
          with TExtFormComboBox(Result) do begin
            AllowBlank     := false;
            ForceSelection := true;
            TriggerAction  := 'all';
            TypeAhead      := true;
            SelectOnFocus  := true;
            Mode           := 'local';
            Enums          := '';
            PropType := Properties[I].PropType{$IFNDEF FPC}^{$ENDIF};
            for J := MinValueProp[I] to MaxValueProp[I] do begin
              Enum := ClearEnumName(GetEnumName(PropType, J));
              EditorLength := max(EditorLength, length(Enum) + Pad);
              Enums := Enums + '["' + IntToStr(J) + '","' + Enum + '"],'
            end;
            Enums := copy(Enums, 1, length(Enums)-1);
            StoreArray := JSArray(Enums);
          end;
        end
        else begin // number
          Result := TExtFormNumberField.Create;
          with TExtFormNumberField(Result) do begin
            AllowDecimals := PascalTypeToJS(Properties[I]) = 'float';
            MinValue := MinValueProp[I];
            MaxValue := MaxValueProp[I];
            if Mask <> '' then begin
              RegEx := Mask;
              EditorLength := LengthRegExp(Mask);
            end
            else
              EditorLength := trunc(Log10(max(abs(MaxValueProp[I]), abs(MinValueProp[I]+1)))+1);
          end;
        end;
      2 : begin
        Result := TExtFormCheckBox.Create; // boolean
        Result.Width := 20;
      end;
      3 : begin // date
        case GetDateTimeType(TypeNameProp[I]) of
          dtDateTime : begin
            Result := TExtFormDateField.Create;
            TExtFormDateField(Result).Format := 'd/m/Y, h:i:s';
            EditorLength := 20;
          end;
          dtTime : begin
            Result := TExtFormTimeField.Create;
            TExtFormTimeField(Result).Format := 'h:i:s';
            EditorLength := 8;
          end;
        else
          Result := TExtFormDateField.Create;
          EditorLength := 10;
        end;
        if Mask <> '' then TExtFormDateField(Result).Format := Mask;
      end;
      4 : begin
        Result := TExtFormTextField.Create; // class
        EditorLength := 10;
      end;
    else
      case CaseOf(copy(lowercase(Mask), 1, 4), ['memo', 'edit', 'file', 'repo', 'mete']) of
        0 : begin
          Result := TExtFormTextArea.Create;
          EditorLength := GetMaskWidth(Mask);
        end;
        1 : begin
          Result := TExtFormHTMLEditor.Create;
          TExtFormHTMLEditor(Result).EnableSourceEdit := false;
          EditorLength := GetMaskWidth(Mask);
        end;
        2 : begin
          Result := TExtFormTextField.Create;
          TExtFormTextField(Result).InputType := itFile;
        end;
      else // string
        if TypeProp[I] = tkSet then begin
          Result := TExtUxFormLovCombo.Create;
          with TExtUxFormLovCombo(Result) do begin
            AllowBlank := false;
            ForceSelection := true;
            TriggerAction := 'all';
            TypeAhead  := true;
            SelectOnFocus := true;
            Mode  := 'local';
            Enums := '';
            PropType := Properties[I].PropType{$IFNDEF FPC}^{$ENDIF};
            CompType := GetTypeData(PropType).CompType{$IFNDEF FPC}^{$ENDIF};
            EditorLength := 1;
            for J := 0 to GetTypeData(CompType).MaxValue do begin
              Enum := ClearEnumName(GetEnumName(CompType, J));
              if J < 4 then inc(EditorLength, length(Enum) + Pad);
              Enums := Enums + '["' + IntToStr(J) + '","' + Enum + '"],';
            end;
            Enums := copy(Enums, 1, length(Enums)-1);
            StoreArray := JSArray(Enums);
          end;
        end
        else begin
          Result := TExtFormTextField.Create;
          with TExtFormTextField(Result) do 
            if Mask <> '' then begin
              RegEx := Mask;
              EditorLength := min(LengthRegExp(Mask), 40);
            end
        end;
      end;
      if EditorLength = 0 then EditorLength := 30;
    end;
    with TExtFormTextField(Result) do begin
      if Width = 0 then Width := JSExpression('%s * %d', [ExtUtilTextMetrics.GetWidth('g'), EditorLength + 1]);
      if I = 0 then
        FieldLabel := Properties[I].Name
      else
        FieldLabel := AliasProp[I];
      Name := Properties[I].Name;
//      On('change', Ajax(ValidateField, ['ID', TExtDataRecord(Selection.GetSelected).Get('ID'), 'Field', '%0.getEl()', 'Value', '%1']));
      if Result is TExtFormTextField then AllowBlank := not InConstraints(I, NotNull);
      ReadOnly := InConstraints(I, epCommon.ReadOnly) or (I = 0);
      if I = 0 then Disabled := true;
      //Disabled := not InConstraints(I, Enabled);
      //if InConstraints(I, Check) then Validator := Ajax();
    end;
  end;
end;

procedure TepThread.DeleteObject;
var
  Prevalent : TTransient;
begin
  Prevalent := PrevalentList.Find(QueryAsInteger['ID']);
  if Prevalent <> nil then Prevalent.Delete;
end;

procedure TepThread.DeleteSelection;
var
  Reg : TExtDataRecord;
begin
  Reg := TExtDataRecord.Init('%0'{Selection.GetSelected});
  with DataStore do begin
    Remove(Reg);
    Ajax(DeleteObject, ['ID', Reg.Get('ID')]);
  end;
end;

procedure TepThread.DeleteSelections; begin
  with Selection do Each(JSFunction(DeleteSelection));
end;

procedure TepThread.LoadData;
var
  Prevalent : TTransient;
  I : integer;
begin
  with PrevalentList do begin
    Prevalent := PrevalentList.First;
    I := 0;
    while (Prevalent <> nil) and (I < QueryAsInteger['start']) do begin
      Next(Prevalent);
      inc(I);
    end;
    I := 0;
    while (Prevalent <> nil) and (I < QueryAsInteger['limit']) do begin
      if Response <> '' then Response := Response + ',';
      Response := Response + Props.PropsToJSON(Prevalent);
      Next(Prevalent);
      inc(I);
    end;
    Response := '{Total:' + IntToStr(PrevalentList.Count) + ',Root:[' + Response + ']}';
  end;
end;

procedure TepThread.AddObject;
var
  Prevalent : TPrevalent;
begin
  BeginTransaction;
  Prevalent := TPrevalentClass(PrevalentList.ObjectClass).Create;
  Prevalent.Add;
  EditorGrid.StopEditing;
  with DataStore do Insert(0, JSArray('new Ext.data.Record(' + Props.PropsToJSON(Prevalent) + ')'));
  EditorGrid.StartEditing(0, 0);
end;

procedure TepThread.BrowseClass;
var
  I, J, Linhas, EditorLength : integer;
  ClassName, EditorSample, ROFields : string;
  FLayout : TStringList;
  HiddenRest : boolean;
begin
  if EditorGrid <> nil then begin
    GridPanel.Remove(EditorGrid);
    DataStore.RemoveAll;
    DataStore.Free;
    EditorGrid.Free(true);
    RecordForm.Free(true);
  end;
  Selection := TExtGridCheckBoxSelectionModel.Create;
  Selection.MoveEditorOnEnter := true;
  EditorGrid := TExtGridEditorGridPanel.Create;
  DataStore := TExtDataJsonStore.Create;
  RecordForm := TExtUxGridRecordForm.Create;
  ClassName := Prevalence.Prevalents[QueryAsInteger['Class']];
  Props := Prevalence.Metadata(ClassName);
  PrevalentList := Prevalence.PrevalentLists(ClassName + 'List');
  with DataStore, Props do
    for I := 0 to PropCount-1 do
      with TExtDataField.AddTo(Fields) do begin
        Name   := Properties[I].Name;
        TypeJS := PascalTypetoJS(Properties[I]);
        //DefaultValue :=
        if TypeJS = 'date' then
          if MaskProp[I] <> '' then
            DateFormat := MaskProp[I]
          else
            case GetDateTimeType(TypeNameProp[I]) of
              dtDateTime : DateFormat := 'd/m/Y, h:i:s';
              dtTime     : DateFormat := 'h:i:s';
            end;
      end;
  Linhas := round((QueryAsInteger['GridHeight'] / 21) - 4.6); //4.1 sem frame
  with DataStore do begin
    Url := '/ExtPascalUML/LoadData';
    RemoteSort := true;
    Root := 'Root';
    TotalProperty := 'Total';
    //SetDefaultSort('', 'ASC');
    Load(JSObject('params:{start:0,limit:' + IntToStr(Linhas)+'},callback:function(){' + Selection.JSName + '.selectFirstRow()}'));
    Selection.On('rowdeselect', Ajax(UpdateObject, ['ID', '%2.get("ID")', 'Changes', ExtUtilJSON.Encode('%2.getChanges()')]));
  end;
  if Browser = brIE then
    Pad := 1
  else
    Pad := 0;
  with EditorGrid do begin
    ViewConfig := TExtGridGridView.Create;
    TExtGridGridView(ViewConfig).EmptyText := '<center><big>Nenhum dado a apresentar</big></center>';
    Store  := DataStore;
    Title  := Props.AliasProp[0];
    Border := false;
    TrackMouseOver := true;
    StripeRows := true;
    ClickstoEdit := 1;
    Frame := true;
    SelModel := Selection;
    AutoScroll := true;
    Height := JSExpression('%s - 2', [GridPanel.GetInnerHeight]);
    AutoWidth  := true;
//    On('validateedit', Ajax(ValidateField, ['ID', TExtDataRecord(Selection.GetSelected).Get('ID'),
//       'Field', '%0.field', 'Value', '%0.value', 'Row', '%0.row']));
    with Props do begin
      //AutoExpandColumn := Properties[Props.PropCount-1].Name;
//      Selection.AddTo(Columns);
      SetLength(Editors, PropCount);
      FLayout := Explode(',', Props.ExtraRTTI[0].PropOrder);
      FormWidth := 0; MaxHeader := 0; HiddenRest := false;
      for I := InitialProp to PropCount-1 do
        with TExtGridColumn.AddTo(Columns) do begin
          Sortable := true;
          Id := Properties[I].Name;
          DataIndex := Properties[I].Name;
          if HintProp[I] <> '' then Tooltip := HintProp[I];
          if I = 0 then
            Header := Id
          else
            if InConstraints(I, NotNull) then
              Header := '<b>' + AliasProp[I] + ' *</b>'
            else
              Header := AliasProp[I];
          if not GetShowAll and (FLayout.Count <> 0) and (FLayout.IndexOf(Id) = -1) then begin
            Hidden := true;
            Hideable := false;
          end;
          Editor := CreateEditor(Props, I, EditorLength);
          Editors[I] := TExtFormField(Editor);
          if Editor is TExtFormNumberField   then Align := alRight else
          if Editor is TExtUxFormLovCombo    then RendererExtFunction := JSFunction('V', 'var E=[' + Enums + '],R=[];T=V.toString().split(",");for(i in E)for(j in T)if(E[i][0]==T[j])R.push(E[i][1]);return R.toString();') else
          if Editor is TExtFormComboBox      then RendererExtFunction := JSFunction('V', 'var E=[' + Enums + '];for(i in E){if(E[i][0]==V){return E[i][1]};};return V;') else
          if Editor is TExtFormCheckbox      then RendererExtFunction := JSFunction('V, P', 'P.css+=" x-grid3-check-col-td";return "<div class=''x-grid3-check-col"+(V?"-on":"")+"''></div>";') else
          if Editor is TExtFormDateField     then RendererExtFunction := ExtUtilFormat.Date('%0', TExtFormDateField(Editor).Format) else
          if Editor is TExtFormTimeField     then RendererExtFunction := ExtUtilFormat.Date('%0', TExtFormTimeField(Editor).Format);
          if not Hidden then begin
            if (length(Header) + 2) >= EditorLength then begin
              EditorSample := Header + IfThen(length(Header) < 10, 'wW', 'W');
              Width := JSExpression(ExtUtilTextMetrics.GetWidth(EditorSample));
            end
            else
              Width := JSExpression('%s * %d', [ExtUtilTextMetrics.GetWidth('g'), EditorLength]);
            J := pos(' ', AliasProp[I]);
            if J = 0 then J := length(AliasProp[I]);
            MaxHeader := max(MaxHeader, J);
            FormWidth := max(FormWidth, EditorLength)
          end;
        end;
      FLayout.Free;
      inc(FormWidth, MaxHeader + 2 + Pad);
      with RecordForm do begin
        if length(Editors) < 10 then
          ColumnCount := 1
        else
          ColumnCount := 2;
        CancelIconCls := 'cancel';
        CancelText    := 'Cancelar';
        OkIconCls     := 'commit';
        FormConfig    := JSObject('width:' + IntToStr(7 * FormWidth * ColumnCount) + ',labelWidth:' + IntToStr(7 * MaxHeader));
        ROFields      := 'ID:true,';
        for I := 1 to length(Editors)-1 do
          if (Editors[I] <> nil) and Editors[I].ReadOnly then
            ROFields := ROFields + Properties[I].Name + ':true,';
        DisabledFields := JSObject(copy(ROFields, 1, length(ROFields)-1));
      end;
      Plugins := RecordForm;
    end;
    TBar := TExtPagingToolbar.Create;
    with TExtPagingToolbar(TBar) do begin
      PageSize := Linhas;
      Store := DataStore;
      DisplayInfo := true;
      with TExtButton.AddTo(Items) do begin
        IconCls := 'add';
        Tooltip := 'Incluir linha';
        Handler := Ajax(AddObject);
      end;
      with TExtButton.AddTo(Items) do begin
        IconCls := 'delete';
        Tooltip := 'Excluir linhas selecionadas';
        Handler := JSFunction(DeleteSelections);
      end;
      with TExtButton.AddTo(Items) do begin
        IconCls := 'edit';
        Tooltip := 'Editar linha selecionada em um formulário';
//        Handler := JSFunction(EditRecordForm);
        Handler := Ajax(EditObject, ['ID', TExtDataRecord(Selection.GetSelected).Get('ID')]);
      end;
      TExtToolbarSeparator.AddTo(Items);
      with TExtButton.AddTo(Items) do begin
        IconCls := 'addfavourite';
        Tooltip := 'Salvar posição da linha atual';
      end;
      with TExtButton.AddTo(Items) do begin
        IconCls := 'gotofavourite';
        Tooltip := 'Ir para a posição salva';
      end;
      TExtToolbarSeparator.AddTo(Items);
      with TExtButton.AddTo(Items) do begin
        IconCls := 'upclass';
        Tooltip := 'Subir classe para 1o. plano';
      end;
      with TExtButton.AddTo(Items) do begin
        IconCls := 'filter';
        Tooltip := 'Filtrar';
      end;
      with TExtButton.AddTo(Items) do begin
        IconCls := 'format';
        Tooltip := 'Mudar formato';
      end;
      with TExtButton.AddTo(Items) do begin
        IconCls := 'print';
        Tooltip := 'Imprimir';
      end;
      TExtToolbarSeparator.AddTo(Items);
      with TExtButton.AddTo(Items) do begin
        IconCls := 'find';
        Tooltip := 'Pesquisar';
      end;
      with TExtButton.AddTo(Items) do begin
        IconCls := 'execute';
        Tooltip := 'Executar operações';
      end;
    end;
    AddTo(GridPanel.Items);
  end;
  GridPanel.DoLayout;
end;

procedure TepThread.OpenObjects;
const
  I : integer = 0;
begin
  inc(I);
  with TExtPanel.AddTo(CentralPanel.Items) do begin
    Id       := 'objects' + IntToStr(I);
    Layout   := lyBorder;
    Title    := 'Objetos em ' + ServerName;
    IconCls  := 'objects';
    Closable := true;
    AutoScroll := true;
    GridPanel  := TExtPanel.Create;
    GridPanel.AutoScroll := true;
    ClassTreePanel.AddTo(Items);
    GridPanel.AddTo(Items).Region := rgCenter;
    Keys := JSObject('ctrl:true,key:"o",fn:function(s,e){e.preventDefault();alert(s + " was pressed");}');
    CentralPanel.Activate(Id);
  end;
end;

procedure TepThread.Home; begin
  Application.Icon := ImagePath + '/extpascal.ico'; // deve ser 16?
  if GetShowID or GetShowAll then
    InitialProp := 0
  else
    InitialProp := 2;
  SetIconCls(['task', 'objects', 'commit', 'cancel', 'refresh', 'info', 'help', 'extpascal', 'exit']);
  SetIconCls(['first', 'prevpage', 'previous', 'next', 'nextpage', 'last', 'add', 'delete', 'edit', 'addfavourite', 'gotofavourite',
              'upclass', 'filter', 'format', 'print', 'find', 'execute', 'back', 'advance', 'package', 'abstract']);
  SetStyle('.CenterCheck{position:absolute;left:10px;}');
  SetStyle('.invalid{border:red solid thin}');
  SetLibrary('/ux/Ext.ux.form.LovCombo', true);
  SetLibrary('/ux/Ext.ux.grid.RecordForm-debug', true);
//  SetLibrary(ExtPath + '/examples/grid/RowExpander');
  LoadUserInfo;
  ExtQuickTips.Init(true);
  InitialPage := nil;
  LoadTaskMenu;
  with TExtViewport.Create do begin
    Layout := lyBorder;
    // Menu principal
    with TExtPanel.AddTo(Items) do begin
      Region := rgNorth;
      Height := 27;
      with TExtToolbar.AddTo(Items) do begin
        Border := false;
        with TExtButton.AddTo(Items) do begin
          Text    := 'Tarefas';
          IconCls := 'task';
          Menu_    := TaskMenu;
        end;
        with TExtButton.AddTo(Items) do begin
          Text    := 'Abrir Objetos';
          Tooltip := 'Ctrl+O - Abrir Objetos';  // F1
          IconCls := 'objects';
          Handler := Ajax(OpenObjects);
        end;
        TExtToolbarSeparator.AddTo(Items);
        with TExtButton.AddTo(Items) do begin
          Text    := 'Confirmar';
          Tooltip := 'Ctrl+S - Confirmar alterações pendentes';
          IconCls := 'commit';
          Handler := Ajax(Commit);
        end;
        with TExtButton.AddTo(Items) do begin
          Text    := 'Cancelar';
          Tooltip := 'Ctrl+Del - Cancelar alterações pendentes'; // 46
          IconCls := 'cancel';
          Handler := Ajax(Cancel);
        end;
        TExtToolbarSeparator.AddTo(Items);
        with TExtButton.AddTo(Items) do begin
          Text    := 'Página inicial';
          IconCls := 'info';
          Handler := Ajax(ShowInitialPage);
        end;
        with TExtButton.AddTo(Items) do begin
          Text    := 'Ajuda';
          Tooltip := 'F1 - Ajuda'; // 112 F1
          IconCls := 'help';
        end;
        with TExtButton.AddTo(Items) do begin
          Text    := 'Sobre';
          IconCls := 'extpascal';
          Handler := Ajax(About);
        end;
      end;
      Keys := JSObject('ctrl:true,key:"o",scope:window,fn:function(s,e){e.preventDefault();alert(s + " was pressed");}');
    end;
    // Status bar
    with TExtPanel.AddTo(Items) do begin
      Id := 'statusbar';
      Region := rgSouth;
      Height := 27;
      Border := false;
      TExtToolbarTextItem.AddTo(BbarArray).SetText('Prevalência ' + PrevalenceVersion);
      Keys := JSObject('ctrl:true,key:"o",fn:function(s,e){e.preventDefault();alert(s + " was pressed");}');
    end;
    // Painel central
    CentralPanel := TExtTabPanel.Create;
    with CentralPanel.AddTo(Items) do begin
      Region := rgCenter;
      EnableTabScroll := true;
      ShowInitialPage;
      Keys := JSObject('ctrl:true,key:"o",fn:function(s,e){e.preventDefault();alert(s + " was pressed");}');
      //on('click', Ajax(FreeInitialPage));
    end;
  end;
  CentralPanel.Focus;
end;

function TepThread.Choose(List: TObjectList; PropOrder, Title, Message: string; Buttons: TBrowserButtons;  TimeOut: integer): TTransient; begin
  Result := nil
end;

procedure TepThread.Cancel; begin
  if DataStore <> nil then begin
    RollBack;
    DataStore.RejectChanges;
    BeginTransaction
  end;
end;

function TepThread.Choose(List: TObjectList; Options: array of string; PropOrder, Title, Message: string; Buttons: TBrowserButtons;
  TimeOut: integer): TTransient;
begin
  Result := nil
end;

procedure TepThread.Commit; begin
  if DataStore <> nil then begin
    EndTransaction;
    DataStore.CommitChanges;
    BeginTransaction
  end;
end;

function TepThread.Confirm(Msg : string; TimeOut : integer) : boolean; begin
  Result := true
end;

function TepThread.Edit(List: TObjectList; PropOrder, Title, Message: string; Buttons: TBrowserButtons; Format: TBrowserEditFormat;
  const ReadOnly: boolean; TimeOut: integer): TBrowserButton;
begin
  Result := bbBack;
end;

procedure TepThread.AddFieldsToForm(Items : TExtObjectList; var HasGroup : boolean);
var
  I, J : integer;
  FLayout  : TStringList;
  SubItems : TExtObjectList;
  TabPanel : TExtTabPanel;
begin
  HasGroup := false;
  TabPanel := nil;
  SubItems := Items;
  FLayout  := Explode(',', Props.ExtraRTTI[0].PropOrder);
  if GetShowAll or (FLayout.Count = 0) then
    for I := InitialProp to Props.PropCount-1 do
      Editors[I].CloneConfig(nil).AddTo(Items)
  else
    for I := 0 to FLayout.Count-1 do begin
      case FLayout[I][1] of
        '<', '>', '|' : begin // FieldSet
          HasGroup := true;
          if FLayout[I][2] in ['<', '>', '|'] then
            SubItems := Items
          else
            with TExtFormFieldSet.AddTo(Items) do begin
              Title := copy(FLayout[I], 2, length(FLayout[I])-2);
              Collapsible := FLayout[I][1] <> '|';
              Collapsed   := FLayout[I][1]  = '>';
              AutoHeight  := true;
              SubItems    := Items;
            end;
          continue;
        end;
        '/' : FLayout[I] := copy(FLayout[I], 2, length(FLayout[I])-2); // CRLF obrigatório
        '[' : begin // TabPanel
          HasGroup := true;
          if FLayout[I][2] = ']' then
            SubItems := Items
          else begin
            if TabPanel = nil then begin
              TabPanel := TExtTabPanel.AddTo(Items);
              TabPanel.ActiveTabNumber := 0;
              TabPanel.Border := false;
              TabPanel.EnableTabScroll := true;
            end;
            with TExtPanel.AddTo(TabPanel.Items) do begin
              Title := copy(FLayout[I], 2, length(FLayout[I])-2);
              AutoHeight := true;
              Frame    := true;
              Layout   := lyForm;
              SubItems := Items;
            end;
          end;
          continue;
        end;
      end;
      for J := InitialProp to high(Editors) do
        if lowercase(Editors[J].Name) = lowercase(FLayout[I]) then begin
          Editors[J].CloneConfig(nil).AddTo(SubItems);
          break
        end;
    end;
  FLayout.Free;
end;

procedure TepThread.EditObject;
var
  HasGroup : boolean;
begin
  with TExtWindow.Create do begin
    Title  := Props.AliasProp[0];
    Modal  := true;
    Shadow := false;
    Constrain := true;
    with TExtFormFormPanel.AddTo(Items) do begin
      LabelWidth := JSExpression('%s * %d', [ExtUtilTextMetrics.GetWidth('g'), MaxHeader]);
      Frame := true;
      AddFieldsToForm(Items, HasGroup);
    end;
    if Browser = brIE then
      Width := JSExpression('Math.max(%s * %d, 200)', [ExtUtilTextMetrics.GetWidth('g'), FormWidth + IfThen(HasGroup, 6, 3)]);
    with TExtButton.AddTo(Buttons) do begin
      IconCls := 'commit';
      Text    := 'Ok';
      Handler := Close;
    end;
    with TExtButton.AddTo(Buttons) do begin
      IconCls := 'cancel';
      Text    := 'Cancelar';
      Handler := Close;
    end;
    Show;
//    Free;
  end;
end;

procedure TepThread.EditRecordForm; begin
  RecordForm.Show(TExtDataRecord(Selection.GetSelected));
end;

procedure TepThread.Error(Message: string); begin

end;

function TepThread.Edit(Obj: TPrevalent; PropOrder, Title, Message: string; Buttons: TBrowserButtons; ReadOnly: boolean;
  TimeOut: integer): TBrowserButton;
begin
  Result := bbBack;
end;

procedure TepThread.ShowMessage(Message: string); begin

end;

procedure TepThread.UpdateObject; begin

end;

procedure TepThread.Mark;
var
  ShowConfig : TExtShowConfig;
begin
  with Editors[InvalidField] do begin
    if (pos('Check', ClassName) <> 0) or (pos('Editor', ClassName) <> 0) then begin
      ShowConfig := TExtShowConfig.Create;
      with ShowConfig do begin
        Title   := 'Erro';
        Msg     := InvalidMessage;
        Icon    := ExtMessageBox.ERROR;
        Buttons := ExtMessageBox.OK;
      end;
      ExtMessageBox.Show(ShowConfig)
    end;
    MarkInvalid(InvalidMessage);
  end;
end;

procedure TepThread.ValidateField; begin
  InvalidField := abs(Props.PropByName(Query['Field']));
  with EditorGrid do begin
    StartEditing(QueryAsInteger['Row'], InvalidField);
    InvalidMessage := 'Mensagem de Erro';
    Ajax(Mark);
  end;
end;

procedure TepThread.Message(Msg: string; Clear: boolean); begin

end;

function TepThread.MessageDlg(Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; TimeOut: integer): integer; begin
  Result := 0;
end;

end.
