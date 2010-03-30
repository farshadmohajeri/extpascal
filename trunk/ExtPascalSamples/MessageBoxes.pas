unit MessageBoxes;

interface

uses
  Ext;

type
  TMessageBoxes = class(TExtPanel)
  private
    procedure ReadButtonJS;
  public
    constructor Create;
  published
    procedure ReadButtonAjax;
  end;

implementation

uses
  ExtPascalUtils, Session, Math;

constructor TMessageBoxes.Create;
var
  ShowConfig : TExtShowConfig;
begin
  inherited;
  SelfSession.SetCodePress;
  Title    := 'Message Boxes';
  Width    := IfThen(SelfSession.Browser = brChrome, 850, 815);
  RenderTo := 'body';
  Frame    := true;
  with TExtButton.AddTo(Buttons) do begin
    Text    := 'Alert Dialog';
    Handler := ExtMessageBox.Alert('Status', 'Changes saved succesfully', Ajax(ReadButtonAjax, ['ButtonID', '%0']));
  end;
  with TExtButton.AddTo(Buttons) do begin
    Text    := 'Confirm Message';
    Handler := ExtMessageBox.Confirm('Confirm', 'Are you sure?', JSFunction(ReadButtonJS));
  end;
  with TExtButton.AddTo(Buttons) do begin
    Text    := 'Prompt Dialog';
    Handler := ExtMessageBox.Prompt('Name', 'Please enter your name:', JSFunction(ReadButtonJS));
  end;
  with TExtButton.AddTo(Buttons) do begin
    Id   := 'ButtonMultiline';
    Text := 'Multi-line prompt dialog';
    ShowConfig := TExtShowConfig.Create;
    with ShowConfig do begin
      Title     := 'Address';
      Msg       := 'Please enter your address:';
      Width     := 300;
      Buttons   := ExtMessageBox.OKCANCEL;
      Multiline := true;
      AnimEl    := 'ButtonMultiline';
      Fn        := JSFunction(ReadButtonJS);
    end;
    Handler := ExtMessageBox.Show(ShowConfig);
  end;
  ShowConfig.Free;
  with TExtButton.AddTo(Buttons) do begin
    Id   := 'Yes/No/Cancel Dialog';
    Text := Id;
    ShowConfig := TExtShowConfig.Create;
    with ShowConfig do begin
      Title   := 'Save Changes?';
      Msg     := 'You are closing a tab that has unsaved changes.<br/>Would you like to save your changes?';
      Icon    := ExtMessageBox.QUESTION;
      Buttons := ExtMessageBox.YESNOCANCEL;
      AnimEl  := Id;
      Fn      := Ajax(ReadButtonAjax, ['ButtonID', '%0']);
    end;
    Handler := ExtMessageBox.Show(ShowConfig);
    ShowConfig.Free;
  end;
  with TExtButton.AddTo(Buttons) do begin
    Id   := 'Progress Dialog';
    Text := Id;
    ShowConfig := TExtShowConfig.Create;
    with ShowConfig do begin
      Title    := 'Please wait';
      Width    := 300;
      Progress := true;
      Msg      := 'Loading items...';
      Wait     := true;
      AnimEl   := Id;
      ProgressText := 'Loading...';
      WaitConfig   := TExtProgressWaitConfig.Create;
      with WaitConfig do begin
        Duration  := 5000;
        Interval  := 500;
        Increment := 11;
        Fn        := ExtMessageBox.Alert('Ok', 'Items loaded.');
        Free;
      end;
    end;
    Handler := ExtMessageBox.Show(ShowConfig);
    ShowConfig.Free;
  end;
  SelfSession.SetStyle('.x-window-dlg .ext-mb-download{background:transparent ' +
    'url(' + SelfSession.ExtPath + '/examples/message-box/images/download.gif) no-repeat top left; height:46px}');
  with TExtButton.AddTo(Buttons) do begin
    Id   := 'Wait Dialog';
    Text := Id;
    ShowConfig := TExtShowConfig.Create;
    with ShowConfig do begin
      Msg    := 'Saving your data, please wait...';
      Width  := 300;
      Wait   := true;
      Icon   := 'ext-mb-download';
      AnimEl := Id;
      ProgressText := 'Saving...';
      WaitConfig   := TExtProgressWaitConfig.Create;
      with WaitConfig do begin
        Duration := 5000;
        Interval := 500;
        Fn       := ExtMessageBox.Hide;
        Free;
      end;
    end;
    Handler := ExtMessageBox.Show(ShowConfig);
    ShowConfig.Free;
  end;
  SelfSession.AddShowSourceButton(Buttons, 'MessageBoxes');
end;

procedure TMessageBoxes.ReadButtonAjax; begin
  ExtMessageBox.Alert('AJAX: Button clicked', 'You clicked the "' + SelfSession.Query['ButtonID'] + '" button')
end;

procedure TMessageBoxes.ReadButtonJS; begin
  ExtMessageBox.Alert('Browser Side: Button clicked', 'You clicked the "%0" button')
end;

end.
