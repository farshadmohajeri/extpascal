unit SimpleLogin;

interface

uses
  Ext;

type
  TSimpleLogin = class(TExtWindow)
    constructor Create;
    procedure CheckLogin;
  end;

implementation

uses
  StrUtils, Session, ExtPascalUtils, ExtForm;

procedure TSimpleLogin.CheckLogin; begin
//  if true {user account verification should be done here} then
    with TExtWindow.Create do begin
      Title    := 'Login';
      Width    := 380;
      Height   := 140;
      Plain    := true;
      Layout   := lyFit;
      Closable := false;
      with TExtPanel.AddTo(Items), SelfSession do begin
        Border    := false;
        BodyStyle := SetPaddings(5, 8);
        HTML      := 'Welcome, ' + AnsiReplaceStr(Query['UserName'], ' ', '&nbsp ') +
                     '.<br/>Password: ' + AnsiReplaceStr(Query['Password'], ' ', '&nbsp ');
        AddShowSourceButton(Buttons, 'CheckLogin');
      end;
      Show;
    end
//  else
//    ExtMessageBox.Alert('Unknown', 'User is not known.');
end;

constructor TSimpleLogin.Create;
var
  UserName, Password : TExtFormTextField;
begin
  inherited;
  SelfSession.SetCodePress;
  Title    := 'Login';
  Width    := 366;
  Height   := 137;
  Plain    := true;
  Layout   := lyFit;
  Closable := false;
  with TExtFormFormPanel.AddTo(Items) do begin
    LabelWidth  := 70;
    Border      := false;
    XType       := xtForm;
    ButtonAlign := baRight;
    BodyStyle   := SetPaddings(5, 5);
    DefaultType := xtTextField;
    Defaults    := JSObject('width: 250');
    UserName    := TExtFormTextField.Create;
    Frame       := true;
    with UserName.AddTo(Items) do begin
      Name       := 'user';
      FieldLabel := 'Username';
      InputType  := itText;
    end;
    Password := TExtFormTextField.Create;
    with Password.AddTo(Items) do begin
      Name       := 'pass';
      FieldLabel := 'Password';
      InputType  := itPassword;
    end;
    with TExtButton.AddTo(Buttons) do begin
      Text    := 'LOGIN';
      Handler := Ajax(SelfSession.CheckLogin, ['UserName', UserName.GetValue, 'Password', Password.GetValue]);
    end;
    SelfSession.AddShowSourceButton(Buttons, 'SimpleLogin');
  end;
end;

end.
